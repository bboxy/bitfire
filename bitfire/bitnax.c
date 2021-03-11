#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <limits.h>
#include <ctype.h>
#include <assert.h>

#include "sfx.h"

// XXX TODO do sfx without overlap and copy data to final position

typedef enum {
	false,
	true
} bool;

#define VERIFY_COST_MODEL  1
#define MATCH_LIMIT 32768-128

#define OUTPUT_NONE	0
#define OUTPUT_SFX	2
#define OUTPUT_LEVEL	3
#define OUTPUT_BITFIRE	4

enum {
	RUN_LIMIT = 0xffff
};

enum {
	 INFINITE_WINDOW = (unsigned) INT_MIN
};

// Some definitions for compiler independence
#ifdef _MSC_VER
#	include <malloc.h>
#	include <io.h>
#else
#	include <sys/stat.h>
#endif

#undef min
#undef max
#define remainder remainder_

// The main crunching structure
typedef struct {
	signed short match_length;
	unsigned short match_offset;
	unsigned short repeat;

	union {
		signed hash_link;
		unsigned cumulative_cost;
	};
} lz_info;

typedef struct {
	unsigned char *src_data;
	unsigned src_begin;
	unsigned src_end;
	unsigned output_begin;
	unsigned output_end;
	bool full_dict;
	signed margin;

	FILE *dst_file;
	unsigned dst_bits;
	unsigned dst_used;

	unsigned file_size;

	lz_info *info;

	signed hash_table[0x100];
	//be graceful, only two max literals is a bit low, as tere can be uncompressibel data rows even bigger than that
	unsigned char dst_literals[65536];
	unsigned output_type;
	unsigned end_pos;
	unsigned header_size;
	signed load_addr;
	signed depack_to;
	unsigned cut_addr_first;
	unsigned cut_addr_last;
	signed start_addr;
	signed relocate_to;

	const char *input_name;
	char *output_name;

	bool is_cbm;
	bool show_trace;
	bool overlap;
	bool output;
	bool checksum;
	bool exit_on_warn;
	bool verbose;
} lz_context;

/******************************************************************************
 * Various utility functions and bithacks
 ******************************************************************************/
#define countof(n) (sizeof(n) / sizeof *(n))

unsigned _log2(unsigned value) {
#	ifdef __GNUC__
	enum { WORD_BITS = sizeof(unsigned) * CHAR_BIT };

	return (WORD_BITS - 1) ^ __builtin_clz(value);
#	else
	signed bits = -1;

	do
		++bits;
	while(value >>= 1);

	return bits;
#	endif
}

bool wraps(unsigned cursor, unsigned length, unsigned limit) {
	return ((cursor + length) ^ cursor) >= limit;
}

unsigned remainder(signed cursor, signed window) {
	return -(cursor | -window);
}

unsigned min(unsigned a, unsigned b) {
	return (a < b) ? a : b;
}

unsigned max(unsigned a, unsigned b) {
	return (a < b) ? b : a;
}

#ifdef _MSC_VER
__declspec(noreturn)
#elif defined(__GNUC__)
__attribute__((noreturn))
__attribute__((format(printf, 1, 2)))
#endif
static void
#ifdef _MSC_VER
__cdecl
#endif

fatal(const char *format, ...) {
	va_list args;

	va_start(args, format);
	fputs("error: ", stderr);
	vfprintf(stderr, format, args);
	fputc('\n', stderr);
	va_end(args);

	exit(EXIT_FAILURE);
}

/******************************************************************************
 * Manage the output stream
 ******************************************************************************/
void _output_doflush(lz_context *ctx) {
	putc(ctx->dst_bits, ctx->dst_file);
	fwrite(ctx->dst_literals, ctx->dst_used, 1, ctx->dst_file);

	ctx->dst_bits = 1;
	ctx->dst_used = 0;
}

void output_open(lz_context *ctx, const char *name) {
	if(ctx->dst_file = fopen(name, "wb"), !ctx->dst_file)
		fatal("cannot create '%s'", name);

	ctx->dst_bits = 1;
	ctx->dst_used = 0;
}

void output_close(lz_context *ctx) {
	if(ctx->dst_bits != 1) {
		while(ctx->dst_bits < 0x100)
			ctx->dst_bits <<= 1;

		putc(ctx->dst_bits, ctx->dst_file);
	}

	fwrite(ctx->dst_literals, ctx->dst_used, 1, ctx->dst_file);
	fclose(ctx->dst_file);
}

void output_bit(lz_context *ctx, unsigned bit) {
	ctx->file_size++;
	if (ctx->output) {
		if(ctx->dst_bits >= 0x100)
			_output_doflush(ctx);

		if (ctx->show_trace) printf("%d", bit & 1);
		ctx->dst_bits <<= 1;
		ctx->dst_bits += bit & 1;
	}
}

void output_literal(lz_context *ctx, unsigned value) {
	ctx->file_size += 8;
	if (ctx->output) {
		ctx->dst_literals[ctx->dst_used++] = value;
	}
}

unsigned output_bitsize(lz_context *ctx) {
	unsigned total;
	unsigned bitbuffer;

	total = ftell(ctx->dst_file);
	total += ctx->dst_used;
	total <<= 3;

	for(bitbuffer = ctx->dst_bits; bitbuffer > 1; bitbuffer >>= 1)
		++total;

	return total;
}


/******************************************************************************
 * Read file into memory and allocate per-byte buffers
 ******************************************************************************/
void read_input(lz_context *ctx, const char *name, bool is_cbm) {
	FILE *file;
	signed length;
	unsigned origin;

	if(file = fopen(name, "rb"), !file)
		fatal("unable to open '%s'", name);

#	ifdef _MSC_VER
	length = _filelength(_fileno(file));
#	else
	{
		struct stat stat;
		stat.st_size = 0;
		fstat(fileno(file), &stat);
		length = stat.st_size;
	}
#	endif

	if(length <= 0)
		fatal("cannot determine length of '%s'", name);

	{
		// Give us a sentinel for the info structure and prevent two-byte
		// hashing from overrunning the buffer
		unsigned count = length + 1;

		ctx->info = malloc(count *
			(sizeof *ctx->info + sizeof *ctx->src_data));
		ctx->src_data = (void *) &ctx->info[count];

		if(!ctx->info)
			fatal("cannot allocate memory buffer");

		if(fread(ctx->src_data, length, 1, file) != 1)
			fatal("cannot read '%s'", name);
	}

	// Deal with the PRG file header. We don't write the loading
	// address back out to compressed file, however we *do* need to
	// consider the decompression address when deciding whether a
	// run crosses a page or window boundary
	origin = 0;

	if(is_cbm) {
		length -= 2;

		if(length < 0) {
			fatal("CBM .prg file is too short to "
				"fit a 2-byte load address");
		}

		origin = *ctx->src_data++;
		origin += *ctx->src_data++ << 8;
	}

	ctx->info -= origin;
	ctx->src_data -= origin;
	ctx->src_begin = origin;
	ctx->src_end = origin + length;
}

void cut_input_addr(lz_context *ctx, unsigned first, unsigned last, bool full_dict) {
	ctx->output_begin = max(ctx->src_begin, first);
	ctx->output_end = min(ctx->src_end, last);
	//if dict shall be restricted to current range only, src end/begin is set to output end/begin
	if (!full_dict) {
		ctx->src_begin = ctx->output_begin;
		ctx->src_end = ctx->output_end;
	}
	if(ctx->src_begin > ctx->src_end)
		fatal("no data in address range $%04x $%04x", ctx->src_begin, ctx->src_end);
}

/******************************************************************************
 * Try to figure out what matches would be the most beneficial
 ******************************************************************************/

unsigned costof_run(unsigned run) {
	return _log2(run) * 2 + 1;
}

unsigned costof_literals(unsigned address, unsigned length) {
        unsigned cost = 1;
        cost += costof_run(length);
        cost += length * 8;
        return cost;
}

unsigned costof_rep(signed offset, unsigned length) {
	unsigned cost = 1;
        cost += costof_run(length);
        return cost;
}

unsigned costof_match(signed offset, unsigned length) {
	unsigned cost = 1;
        cost += costof_run(length - 1);
        cost += 7 + costof_run((offset-1)/128+1);
        return cost;
}

lz_info optimal_parsing_literal(lz_context *ctx, const lz_info *info, unsigned cursor) {
	signed length;
	unsigned cost;

	lz_info result;

	length = -info[cursor + 1].match_length;

	if(length > 0)
		cost = info[cursor + ++length].cumulative_cost;
	else {
		cost = info[cursor + 1].cumulative_cost;
		length = 1;
	}

	cost += costof_literals(cursor, length);

	result.match_length = -length;
	result.cumulative_cost = cost;

	return result;
}

lz_info optimal_parsing (lz_context *ctx, const lz_info *info, unsigned cursor, signed match_offset, unsigned match_length, unsigned match_limit, lz_info best_match, unsigned last_offset, bool allow_repeat) {
	unsigned cost;
	do {
		//if (allow_repeat && match_offset == last_offset) {
		//	cost = costof_rep(match_offset, match_length);
		//	best_match.repeat = 1;
		//} else {
			cost = costof_match(match_offset, match_length);
			best_match.repeat = 0;
		//}
		cost += info[cursor + match_length].cumulative_cost;

		if(cost < best_match.cumulative_cost) {
			best_match.match_offset = match_offset;
			best_match.match_length = match_length;
			best_match.cumulative_cost = cost;
		}
	} while(++match_length <= match_limit);

	return best_match;
}

/******************************************************************************
 * Determine the longest match for every position of the file
 ******************************************************************************/
signed *hashof(lz_context *ctx, unsigned a, unsigned b) {
	static const unsigned char random[] = {
		0x17, 0x80, 0x95, 0x4f, 0xc7, 0xd1, 0x15, 0x13,
		0x91, 0x57, 0x0f, 0x47, 0xd0, 0x59, 0xab, 0xf0,
		0xa7, 0xf5, 0x36, 0xc0, 0x24, 0x9c, 0xed, 0xfd,
		0xd4, 0xf3, 0x51, 0xb4, 0x8c, 0x97, 0xa3, 0x58,
		0xcb, 0x61, 0x78, 0xb1, 0x3e, 0x7e, 0xfb, 0x41,
		0x39, 0xa6, 0x8e, 0x10, 0xa1, 0xba, 0x62, 0xcd,
		0x94, 0x02, 0x0d, 0x2b, 0xdb, 0xd7, 0x44, 0x16,
		0x29, 0x4d, 0x68, 0x0a, 0x6b, 0x6c, 0xa2, 0xf8,
		0xc8, 0x9f, 0x25, 0xca, 0xbd, 0x4a, 0xc2, 0x35,
		0x53, 0x1c, 0x40, 0x04, 0x76, 0x43, 0xa9, 0xbc,
		0x46, 0xeb, 0x99, 0xe9, 0xf6, 0x5e, 0x8f, 0x8a,
		0xf1, 0x5d, 0x21, 0x33, 0x0b, 0x82, 0xdf, 0x52,
		0xea, 0x27, 0x22, 0x9a, 0x6f, 0xad, 0xe5, 0x83,
		0x11, 0xbe, 0xa4, 0x85, 0x1d, 0xb3, 0x77, 0xf4,
		0xef, 0xb7, 0xf2, 0x03, 0x64, 0x6d, 0x1b, 0xee,
		0x72, 0x08, 0x66, 0xc6, 0xc1, 0x06, 0x56, 0x81,
		0x55, 0x60, 0x70, 0x8d, 0x23, 0xb2, 0x65, 0x5b,
		0xff, 0x4c, 0xb9, 0x7a, 0xd6, 0xe6, 0x19, 0x9b,
		0xb5, 0x49, 0x7d, 0xd8, 0x45, 0x1a, 0x84, 0x32,
		0xdd, 0xbf, 0x9e, 0x2f, 0xd2, 0xec, 0x92, 0x0e,
		0xe8, 0x7c, 0x7f, 0x00, 0x86, 0xde, 0xb6, 0xcf,
		0x05, 0x69, 0xd5, 0x37, 0xe4, 0x30, 0x3c, 0xe1,
		0x4b, 0xaa, 0x3b, 0x2d, 0xda, 0x5c, 0xcc, 0x67,
		0x20, 0xb0, 0x6a, 0x1f, 0xf9, 0x01, 0xac, 0x2e,
		0x71, 0xf7, 0xfc, 0x3f, 0x42, 0xd3, 0xbb, 0xa8,
		0x38, 0xce, 0x12, 0x96, 0xe2, 0x14, 0x87, 0x4e,
		0x63, 0x07, 0xae, 0xdc, 0xa5, 0xc9, 0x0c, 0x90,
		0xe7, 0xd9, 0x09, 0x2a, 0xc4, 0x3d, 0x5a, 0x34,
		0x8b, 0x88, 0x98, 0x48, 0xfa, 0xc3, 0x26, 0x75,
		0xfe, 0xa0, 0x7b, 0x50, 0x2c, 0x89, 0x18, 0x9d,
		0x3a, 0x73, 0x6e, 0x5f, 0xc5, 0xaf, 0xb8, 0x74,
		0x93, 0xe3, 0x79, 0x28, 0xe0, 0x1e, 0x54, 0x31
	};

	size_t bucket = random[a] ^ b;
	return &ctx->hash_table[bucket];
}

void generate_hash_table(lz_context *ctx) {
	unsigned cursor;

	const unsigned src_end = ctx->src_end;
	const unsigned char *src_data = ctx->src_data;
	lz_info *info = ctx->info;

	for(cursor = 0; cursor < countof(ctx->hash_table); ++cursor)
		ctx->hash_table[cursor] = INT_MIN;

	for(cursor = ctx->src_begin; cursor != src_end; ++cursor) {
		signed *hash_bucket = hashof (
			ctx,
			src_data[cursor + 0],
			src_data[cursor + 1]
		);

		info[cursor].hash_link = *hash_bucket;
		*hash_bucket = cursor;
	}
}

unsigned find_matches(lz_context *ctx) {
	const unsigned src_begin = ctx->src_begin;
	const unsigned src_end = ctx->src_end;
	const unsigned char *src_data = ctx->src_data;
	lz_info *info = ctx->info;

	unsigned offset_limit = min(INFINITE_WINDOW, MATCH_LIMIT);
	unsigned cursor = ctx->src_end;
        unsigned last_offset = 1;
	bool allow_repeat = true;

	// omit first type bit, start with -1
	info[cursor].cumulative_cost = -1;

	while(cursor != src_begin) {
		unsigned match_length;
		signed cursor_limit;
		unsigned length_limit = 65536;
		signed *hash_bucket;
		signed hash_link;
		lz_info best_match;

		--cursor;

		match_length = 1;
		cursor_limit = cursor - offset_limit;

		length_limit = min(length_limit, remainder(cursor, INFINITE_WINDOW));
		length_limit = min(length_limit, src_end - cursor);

		hash_bucket = hashof (
			ctx,
			src_data[cursor + 0],
			src_data[cursor + 1]
		);

		assert((unsigned) *hash_bucket == cursor);
		hash_link = info[cursor].hash_link;
		*hash_bucket = hash_link;

		best_match = optimal_parsing_literal(ctx, info, cursor);

		while(hash_link >= cursor_limit) {
			unsigned match_limit = remainder(hash_link, INFINITE_WINDOW);
			match_limit = min(match_limit, length_limit);

			if(match_length != match_limit) {
				unsigned i = match_length + 1;

				if(!memcmp(&src_data[cursor], &src_data[hash_link], i)) {
					for(; i != match_limit; ++i) {
						if(src_data[cursor + i] != src_data[hash_link + i])
							break;
					}

					assert(i <= match_limit);

					best_match = optimal_parsing (
						ctx,
						info,
						cursor,
						cursor - hash_link,
						match_length + 1,
						i,
						best_match,
						last_offset,
						allow_repeat
					);

					match_length = i;
				}
			}

			hash_link = info[hash_link].hash_link;
		}
		info[cursor] = best_match;
		if(best_match.match_offset) {
			last_offset = best_match.match_offset;
			allow_repeat = false;
		} else {
			allow_repeat = true;
		}
	}
	//info[src_begin].cumulative_cost--;
        //printf("%d\n", info[src_begin].cumulative_cost);
	return info[src_begin].cumulative_cost;
}


/******************************************************************************
 * Write the generated matches and literal runs
 ******************************************************************************/

int bit_size(int value) {
    int bits = 1;
    while (value > 1) {
        bits++;
        value >>= 1;
    }
    return bits;
}

void write_interlaced_elias_gamma(lz_context *ctx, int value, int skip) {
    int bits = bit_size(value);
    int i;

    for (i = 2; i <= value; i <<= 1)
        ;
    i >>= 1;


    if (bits >= 9) {
        /* change bit-order, send LSB first */
        /* remove preceeding 1 first */
        value = value & ((0xffff ^ i));
        /* move LSB bits to the beginning */
        value = (value >> 8) | ((value & 0xff) << (bits - 9));
    }

    while ((i >>= 1) > 0) {
        if (!skip) output_bit(ctx, 0);
	skip = 0;
        if (!skip) output_bit(ctx, (value & i) != 0);
	skip = 0;
    }
    if (!skip) output_bit(ctx, 1);
}

void encode_literals_plain(lz_context *ctx, unsigned cursor, unsigned length) {
	const unsigned char *data;
	unsigned start = length;

	if(ctx->show_trace) {
		printf ("plain literal(%u bytes)\n",length);
	}

	data = &ctx->src_data[cursor];
	do
		output_literal(ctx, data[start - length--]);
	while(length);
}

void encode_literals(lz_context *ctx, unsigned cursor, unsigned length) {
	const unsigned char *data;
	unsigned start = length;

	if(ctx->show_trace) {
		printf ("literal(%u bytes)\n",length);
	}

	if (ctx->src_begin != cursor) output_bit(ctx, 0);

        write_interlaced_elias_gamma(ctx, length, 0);

	data = &ctx->src_data[cursor];
	do
		output_literal(ctx, data[start - length--]);
	while(length);
}

void encode_rep(lz_context *ctx, signed offset, unsigned length) {
	if(ctx->show_trace) {
		printf("rep(-%u, %u bytes)\n",offset,length);
	}

        output_bit(ctx, 0);
        write_interlaced_elias_gamma(ctx, length, 0);
}

void encode_match(lz_context *ctx, signed offset, unsigned length) {
	if(ctx->show_trace) {
		printf("match(-%u, %u bytes)\n",offset,length);
	}

        output_bit(ctx, 1);
        write_interlaced_elias_gamma(ctx, (offset - 1) / 128 + 1, 0);

        output_literal(ctx, (((offset - 1) % 128) << 1) | (length == 2));

        write_interlaced_elias_gamma(ctx, length - 1, 1);
}

void render_output(lz_context *ctx) {
	unsigned cursor;

//	unsigned output_end = ctx->output_end;
	signed dest_pos;
	signed stream_pos;

	lz_info *info = ctx->info;
	signed length;
	signed length_bit;
	signed packed_size;
	signed last_match;

	bool update = true;

	bool sentinel_needed = false;

	ctx->margin = 0;
	ctx->end_pos = 0;

	packed_size = ctx->file_size;
	last_match = 0;

	ctx->file_size = 0;
	length = 0;

	for(cursor = ctx->output_begin; cursor < ctx->output_end; cursor += length) {
		length = info[cursor].match_length;

		//check for normal overlap
		if (ctx->output) {
			//save pointer before new match/literal is written onto stream
			stream_pos = (packed_size + 7) / 8 - (ctx->file_size + 7) / 8;
			//distance to end from current destination
			dest_pos = ctx->output_end - cursor;
			if(stream_pos > dest_pos && stream_pos - dest_pos > ctx->margin) {
				ctx->margin = stream_pos - dest_pos;
			}
			//only allow unencoded last literal if zero overlap and if load_addr && depack_to is yet unset (--load-addr/--dpeack-to not set)
			if(ctx->load_addr < 0 && ctx->depack_to < 0 && !ctx->overlap && stream_pos > dest_pos) {
				if(update) {
					//last sane position, now switch to literal output only
					ctx->end_pos = cursor;
					update = false;
				}
			}
		}

		if(length > 0) {

			unsigned offset = info[cursor].match_offset;
			sentinel_needed = false;

			if (!update) {
				encode_literals_plain(ctx, cursor, length);
			} else {
				if (info[cursor].repeat) {
					encode_rep(ctx, offset, length);
				} else {
					encode_match(ctx, offset, length);
				}
			}
			if(ctx->show_trace) printf ("\n");

			last_match = cursor + length;
		} else {
			sentinel_needed = true;
			length = -length;

			if(ctx->show_trace) printf ("type_bit ");
			if(update && ctx->output_type != OUTPUT_BITFIRE) output_bit(ctx, 1);
			if(ctx->show_trace) printf ("\n");

			if (!update) {
				encode_literals_plain(ctx, cursor, length);
			} else {
				encode_literals(ctx, cursor, length);
			}
			if(ctx->show_trace) printf ("\n");
		}
	}

	// If still no end_position is determined, do it now
	if (update) ctx->end_pos = last_match;

#	if VERIFY_COST_MODEL
	{
		unsigned expected = info[ctx->output_begin].cumulative_cost;
		unsigned actual = ctx->file_size;

		//if(expected != actual) {
			printf (
				"expected: %u\n"
				"actual:   %u\n",
				expected,
				actual
			);
		//}
	}
#	endif

	//check for normal overlap
	if (ctx->output) {
		//save pointer before new match/literal is written onto stream
		stream_pos = (packed_size + 7) / 8 - (ctx->file_size + 7) / 8;
		//distance to end from current destination
		dest_pos = ctx->output_end - cursor;
		if(stream_pos > dest_pos && stream_pos - dest_pos > ctx->margin) {
			ctx->margin = stream_pos - dest_pos;
		}
	}


	// We encode the EOF whenever we do compress with overlap or to an alternative location where in place depacking can't happen
	if(ctx->overlap || ctx->load_addr >= 0 || ctx->depack_to >= 0) {
		// In that case take end of data as end_pos to disable the end_pos check by letting it hit too late during decoding
		// XXX TODO can still be omitted if last action was a match
		ctx->end_pos = ctx->output_end;

		// only add a sentinael if last action was a literal
		if (sentinel_needed || ctx->overlap || ctx->load_addr >= 0 || ctx->depack_to >= 0) {
			// The sentinel is a maximum-length match
			if(ctx->show_trace) printf("EOF\n");

//			printf("%d bits saved\n", saved);

			length_bit = _log2(RUN_LIMIT);
			output_bit(ctx, --length_bit >= 0);

			while(length_bit >= 0) {
				output_bit(ctx, RUN_LIMIT >> length_bit);
				output_bit(ctx, --length_bit < 0);
			}
		}
	}

	return;
}

/******************************************************************************
 * Helper functions
 ******************************************************************************/
signed read_number(char* arg) {
	if(arg[0] == '$') return strtoul(arg + 1, NULL, 16);
	else if(arg[0] == '0' && arg[1] == 'x') return strtoul(arg + 2, NULL, 16);
	return strtoul(arg, NULL, 10);
}

/******************************************************************************
 *
 ******************************************************************************/

int crunch(lz_context* ctx) {
	unsigned packed_size;
	unsigned source_size;
	unsigned decruncher_size = sizeof decruncher;
	unsigned data_addr = 0;
	int i;

	unsigned char checksum;

	read_input(ctx, ctx->input_name, ctx->is_cbm);
	cut_input_addr(ctx, ctx->cut_addr_first, ctx->cut_addr_last, ctx->full_dict);

	source_size = ctx->src_end - ctx->src_begin;

	// Do the compression
	generate_hash_table(ctx);
	find_matches(ctx);

	output_open(ctx, ctx->output_name);

	if(ctx->output_type == OUTPUT_LEVEL || ctx->output_type == OUTPUT_BITFIRE) {
		// Add 2 blank bytes here first, as the address can not be calculated yet
		fputc(0, ctx->dst_file);
		fputc(0, ctx->dst_file);

		// Add depack address
		fputc(0, ctx->dst_file);
		fputc(0, ctx->dst_file);

		if (!ctx->overlap) {
			// Add 2 blank bytes here first, as the end_addr can not be calculated yet
			fputc(0, ctx->dst_file);
			fputc(0, ctx->dst_file);
		}
	}

	if(ctx->output_type == OUTPUT_SFX) {
		// Output decruncher with loadadress 0x801
		fwrite(decruncher, decruncher_size, 1, ctx->dst_file);

		// Add depack address
		fputc(ctx->start_addr & 0xff, ctx->dst_file);
		fputc(ctx->start_addr >> 8, ctx->dst_file);
	}

	// avoid to enable depack_to mechanisms when src-addr is already depack-destination
	if(ctx->depack_to == ctx->src_begin) {
		ctx->depack_to = -1;
	}

        // XXX TODO not necessary if we can rely on our cost_model
	// Do a dry run with rendering output to find out packed_filesize
	ctx->output = false;
	render_output(ctx);

	// Now go live and find perfect safety margin, and write compressed data to file
	ctx->output = true;
	render_output(ctx);

	output_close(ctx);

	// Now open compressed file and manipulate all kind of header data
	if(ctx->dst_file = fopen(ctx->output_name, "rb+"), !ctx->dst_file) fatal("cannot create '%s'", ctx->output_name);

	// Packed size including all the header bytes
	fseek(ctx->dst_file, 0L, SEEK_END);
	packed_size = ftell(ctx->dst_file);

	// Ignore margin in case of zero overlap
	if (!ctx->overlap) ctx->margin = 0;

	// Adapt size for different output types
	if(ctx->output_type == OUTPUT_LEVEL || ctx->output_type == OUTPUT_BITFIRE) {
		// File contains a load address, subtract
		packed_size -= 2;
	} else if(ctx->output_type == OUTPUT_SFX) {
		// Includes already the load address
		packed_size -= decruncher_size;
	}

	// Emit margin only when necessary
	if (ctx->margin > 0 && ctx->load_addr < 0 && ctx->depack_to < 0 && ctx->output_type != OUTPUT_SFX) {
		if (ctx->verbose) printf("overlap: %d bytes\n", ctx->margin);
	}

	// Some stats and info
	if (ctx->verbose) printf("source size: $%04x (%d)\n", source_size, source_size);
	if (ctx->verbose) printf("packed size: $%04x (%d) %s ratio: %.1f%%\n", packed_size, packed_size, "", (packed_size * 100.0 / (ctx->output_end - ctx->output_begin)));

	// Print more info and calc addresses
	if(ctx->output_type == OUTPUT_LEVEL || ctx->output_type == OUTPUT_BITFIRE) {
		//find perfect loading address
		if (ctx->load_addr < 0) ctx->load_addr = ctx->output_end - packed_size + ctx->margin;
		if (ctx->relocate_to >= 0 && ctx->load_addr >= 0) {
			ctx->load_addr = ctx->load_addr + (ctx->relocate_to - ctx->output_begin);
			ctx->end_pos = ctx->end_pos + (ctx->relocate_to - ctx->output_begin);
		}

		if (ctx->verbose) printf("source load: $%04x-$%04x\n", ctx->output_begin, ctx->output_end);
		if (ctx->verbose) printf("packed load: $%04x-$%04x\n", ctx->load_addr, ctx->load_addr + packed_size);

		// Fix optimal load address to file
		fseek(ctx->dst_file, 0, SEEK_SET);
		fputc(ctx->load_addr & 0xff, ctx->dst_file);
		fputc(ctx->load_addr >> 8, ctx->dst_file);
		// Add depack address
		if (ctx->depack_to >=0) {
			fputc(ctx->depack_to >> 8, ctx->dst_file);
			fputc(ctx->depack_to & 0xff, ctx->dst_file);
		} else {
			if (ctx->relocate_to >= 0) {
				fputc(ctx->relocate_to >> 8, ctx->dst_file);
				fputc(ctx->relocate_to & 0xff, ctx->dst_file);
			} else {
				fputc(ctx->output_begin >> 8, ctx->dst_file);
				fputc(ctx->output_begin & 0xff, ctx->dst_file);
			}
		}
		if (!ctx->overlap) {
			// Add end address
			fputc(ctx->end_pos >> 8, ctx->dst_file);
			fputc(ctx->end_pos & 0xff, ctx->dst_file);
		}

		// Do some sanity checks
		if (ctx->load_addr + packed_size + ctx->margin < ctx->output_end) {
			if (ctx->load_addr + packed_size + ctx->margin >= ctx->output_begin) {
				fprintf(stderr,"WARNING: packed file location collides with depacked data location\n");
				if (ctx->exit_on_warn) exit(EXIT_FAILURE);
			}
		}
		if (packed_size > ctx->output_end - ctx->output_begin) {
			fprintf(stderr,"WARNING: compressed file is bigger than original\n");
			if (ctx->exit_on_warn) exit(EXIT_FAILURE);
		}
		if ((ctx->load_addr > 0xd000 && ctx->load_addr < 0xe000) || (ctx->load_addr + packed_size > 0xd000 && ctx->load_addr + packed_size < 0xe000)) {
			fprintf(stderr,"WARNING: resulting file has parts in IO-range $d000-$dfff\n");
			if (ctx->exit_on_warn) exit(EXIT_FAILURE);
		}
		if (ctx->output_type == OUTPUT_BITFIRE) {
			if (ctx->verbose) printf("filetype: bitfire\n");
		} else {
			if (ctx->verbose) printf("filetype: level\n");
		}
	} else if(ctx->output_type == OUTPUT_SFX) {
		packed_size -= 26;
		data_addr = decruncher_size + packed_size - 0xff + 0x800 + 24;

		// Set up depacker values
		fseek(ctx->dst_file, BITNAX_SIZE_HI, SEEK_SET);
		fputc((packed_size >> 8) + 1, ctx->dst_file);

		fseek(ctx->dst_file, BITNAX_DATA_ADDR, SEEK_SET);
		fputc(data_addr & 0xff, ctx->dst_file);
		fputc(data_addr >> 8, ctx->dst_file);

		fseek(ctx->dst_file, BITNAX_SIZE_LO, SEEK_SET);
		fputc((0x10000 - packed_size) & 0xff, ctx->dst_file);

		fseek(ctx->dst_file, BITNAX_SECTOR_PTR_2, SEEK_SET);
		fputc((0x10000 - packed_size) >> 8, ctx->dst_file);
		fseek(ctx->dst_file, BITNAX_SECTOR_PTR_3, SEEK_SET);
		fputc((0x10000 - packed_size) >> 8, ctx->dst_file);
		fseek(ctx->dst_file, BITNAX_SECTOR_PTR_1, SEEK_SET);
		fputc((0x10000 - packed_size) >> 8, ctx->dst_file);

		fseek(ctx->dst_file, BITNAX_DEST_ADDR, SEEK_SET);
		fputc(ctx->output_begin & 0xff, ctx->dst_file);
		fputc(ctx->output_begin >> 8, ctx->dst_file);

		if (ctx->verbose) printf("start address: $%04x (%d)\n", ctx->start_addr, ctx->start_addr);
		if (ctx->verbose) printf("final size: $%04x (%d)\n", packed_size + decruncher_size, packed_size + decruncher_size);
		if (ctx->verbose) printf("filetype: sfx\n");
	} else {
	}

	fclose(ctx->dst_file);

	if (ctx->checksum) {
		checksum = 0;
		for (i = ctx->output_begin; i < ctx->output_end; i++) {
			checksum ^= ctx->src_data[i];
		}
		printf("checksum: $%02x\n", checksum);
	}

	// Display some statistics gathered in the process
	return EXIT_SUCCESS;
}

/******************************************************************************
 * The main function
 ******************************************************************************/
int
#ifdef _MSC_VER
__cdecl
#endif
main(int argc, char *argv[]) {
	const char *program_name;
	unsigned name_length;
	lz_context ctx;

	// Parse the command line
	program_name = *argv;

	ctx.output_name = NULL;
	ctx.cut_addr_first = 0;
	ctx.cut_addr_last = INT_MAX;

        ctx.is_cbm = true;
	ctx.show_trace = false;
	ctx.output_type = OUTPUT_LEVEL;
	ctx.full_dict = false;
	ctx.overlap = false;
	ctx.header_size = 0;
	ctx.checksum = false;
	ctx.exit_on_warn = false;
	ctx.load_addr = -1;
	ctx.depack_to = -1;
	ctx.start_addr = -1;
	ctx.relocate_to = -1;
        ctx.verbose = false;

	while(++argv, --argc) {
		if(argc >= 2 && !strcmp(*argv, "-o")) {
			ctx.output_name = *++argv;
			--argc;
		} else if(argc >= 2 && !strcmp(*argv, "--sfx")) {
			ctx.start_addr = read_number(*++argv);
			ctx.output_type = OUTPUT_SFX;
			--argc;
		} else if(argc >= 2 && !strcmp(*argv, "--load-addr")) {
			ctx.load_addr = read_number(*++argv);
			--argc;
		} else if(argc >= 2 && !strcmp(*argv, "--depack-to")) {
			ctx.depack_to = read_number(*++argv);
			--argc;
		} else if(argc >= 2 && !strcmp(*argv, "--relocate-to")) {
			ctx.relocate_to = read_number(*++argv);
			--argc;
		} else if(!strcmp(*argv, "--level")) {
			ctx.output_type = OUTPUT_LEVEL;
		} else if(!strcmp(*argv, "--bitfire")) {
			ctx.output_type = OUTPUT_BITFIRE;
		} else if(argc >= 3 && !strcmp(*argv, "--cut-input-addr")) {
			ctx.cut_addr_first = read_number(*++argv);
			ctx.cut_addr_last = read_number(*++argv);
			if(ctx.cut_addr_first > ctx.cut_addr_last)
				fatal("last must be larger than first");
			argc -= 2;
		} else if(!strcmp(*argv, "--full-dict")) {
			ctx.full_dict = true;
		} else if(!strcmp(*argv, "-v")) {
			ctx.verbose = true;
		} else if(!strcmp(*argv, "--overlap")) {
			ctx.overlap = true;
		} else if(!strcmp(*argv, "--trace-coding")) {
			ctx.show_trace = true;
		} else if(!strcmp(*argv, "--binfile")) {
			ctx.is_cbm = false;
		} else if(!strcmp(*argv, "--checksum")) {
			ctx.checksum = true;
		} else if(!strcmp(*argv, "--exit-on-warn")) {
			ctx.exit_on_warn = true;
		} else {
			break;
		}
	}

	if(argc != 1) {
		fprintf (
			stderr,
			"syntax: %s\n"
			"\t[-o output.lz                                        selects output file\n"
			"\t[--overlap]                                          allow overlap and disable in place depacking\n"
			"\t[--sfx startaddr]                                    create a sfx with given startaddress\n"
			"\t[--level]                                            create a level packed file\n"
			"\t[--bitfire]                                          create a file compatible with bitfire\n"
			"\t[--full-dict]                                        allow matches over whole file\n"
			"\t[--cut-input-addr first last]                        only pack selected part of file\n"
			"\t[--load-addr addr]                                   change load address of packed file\n"
			"\t[--depack-to addr]                                   change depack address of file\n"
			"\t[--relocate-to addr]                                 change load address of file, prior to packing\n"
			"\t[--binfile]                                          file has no 2 byte load-address (--relocate-to can help)\n"
			"\t[--checksum]                                         print eor checksum over file\n"
			"\t[--exit_on_warn]                                     exit with error instead of giving a warning\n"
			"\t[-v]                                                 print details\n"
			"\t[--trace-coding]                                     print more gibberish\n"
			"\t{input-file}                                         the input file\n",
			program_name
		);
		return EXIT_FAILURE;
	}

	ctx.input_name = *argv;

	if (!ctx.is_cbm) {
		if (ctx.relocate_to < 0) {
			fatal("a load address is needed via the --relocate-to switch");
		}
	}

	if (ctx.output_type == OUTPUT_SFX) {
		if (!ctx.overlap) {
			ctx.overlap = true;
			//fprintf(stderr,"ignoring --overlap option for filetype --sfx");
		}
		if (ctx.depack_to >= 0) {
			fatal("option --depack-to not supported with --sfx");
		}
		if (ctx.load_addr >= 0) {
			fatal("option --load-addr not supported with --sfx");
		}
		if (ctx.relocate_to >= 0) {
			fatal("option --relocate-to not supported with --sfx");
		}
	}

	// If necessary generate output file by substituting the
	// extension for .lz
	if(!ctx.output_name) {
		static const char extension[] = ".lz";
		name_length = strlen(ctx.input_name);

		ctx.output_name = malloc(name_length + 1 + sizeof extension);

		memcpy(ctx.output_name, ctx.input_name, name_length);
		memcpy(ctx.output_name + name_length, extension, sizeof extension);
	}

	return crunch(&ctx);
}
