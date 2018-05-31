#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <limits.h>
#include <ctype.h>
#include <assert.h>

typedef enum {
	false,
	true
} bool;

#define VERIFY_COST_MODEL  0
#define DEFAULT_LENGTHS    "3/6/8/10:4/7/10/13"
#define BITFIRE_WITH_MOFF  0

#define OUTPUT_NONE	0
#define OUTPUT_SFX	2
#define OUTPUT_LEVEL	3
#define OUTPUT_BITFIRE	4

enum {
	RUN_LIMIT = 0x100,
	OFFSET_LENGTH_LIMIT = 15
};

enum {
	 INFINITE_WINDOW = (unsigned) INT_MIN
};

static const char decruncher[] = {
0x01,0x08,0x1c,0x08,0x00,0x00,0x9e,0x32,0x30,0x37,0x38,0x3a,0x22,0x14,0x14,0x14,
0x14,0x14,0x14,0x14,0x14,0x14,0x42,0x49,0x54,0x4e,0x41,0x58,0x00,0x00,0x00,0x78,
0xe6,0x01,0xa2,0x00,0xbd,0x46,0x08,0x9d,0x20,0x00,0xe8,0xd0,0xf7,0xa0,0x01,0xca,
0xbd,0x2c,0x08,0x9d,0x00,0xff,0x8a,0xd0,0xf6,0xce,0x31,0x08,0xce,0x34,0x08,0x88,
0xd0,0xed,0xa2,0x1a,0x4c,0x20,0x00,0x38,0x20,0xef,0x00,0x90,0x3b,0xf0,0xf9,0xa9,
0x00,0x2a,0x06,0xee,0xd0,0x03,0x20,0xef,0x00,0x90,0x09,0x06,0xee,0xd0,0xf2,0x20,
0xef,0x00,0xd0,0xed,0x85,0x4f,0xa0,0x00,0xbd,0x00,0x00,0xe8,0xd0,0x03,0x20,0xf9,
0x00,0x99,0x00,0x38,0xc8,0xc0,0x00,0xd0,0xef,0x98,0xd0,0x03,0x4c,0xe7,0x00,0x18,
0x65,0x4b,0x85,0x4b,0x90,0x02,0xe6,0x4c,0xa9,0x01,0x06,0xee,0xd0,0x03,0x20,0xef,
0x00,0xb0,0x17,0x06,0xee,0xd0,0x03,0x20,0xef,0x00,0x90,0x0c,0x06,0xee,0xd0,0x03,
0x20,0xef,0x00,0x2a,0x90,0xed,0xb0,0x7f,0x69,0x01,0x85,0xdd,0xa9,0x20,0x2a,0x06,
0xee,0xd0,0x03,0x20,0xef,0x00,0x2a,0x90,0xf6,0xa8,0xb9,0x08,0x01,0xf0,0x10,0x06,
0xee,0xd0,0x07,0x84,0xa2,0x20,0xef,0x00,0xa0,0x00,0x2a,0x90,0xf2,0x30,0x1f,0x85,
0xc0,0xbd,0x00,0x00,0xe8,0xd0,0x03,0x20,0xf9,0x00,0x79,0x10,0x01,0xb0,0x03,0xc6,
0xc0,0x38,0x65,0x4b,0x85,0xd8,0xa9,0x00,0x79,0x18,0x01,0x38,0xb0,0x09,0x79,0x10,
0x01,0x65,0x4b,0x85,0xd8,0xa9,0xff,0x65,0x4c,0x85,0xd9,0xa0,0xff,0xc8,0xb9,0x00,
0x10,0x91,0x4b,0xc0,0xff,0xd0,0xf6,0x98,0x65,0x4b,0x85,0x4b,0x90,0x02,0xe6,0x4c,
0x06,0xee,0x4c,0x24,0x00,0x00,0xbc,0x00,0x00,0x84,0xee,0x26,0xee,0xe8,0xd0,0x06,
0xe6,0xf1,0xe6,0x43,0xe6,0xac,0x60,0xc6,0x01,0x58,0x85,0x98,0x4c
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
	const char *emit_offset_tables;
	unsigned iterations;

	bool offset_lengths;
	bool write_tables;
	bool show_stats;
	bool is_cbm;
	bool show_trace;
	bool overlap;
	bool output;
	bool checksum;
	bool exit_on_warn;

	// Some informational counters
	struct {
		unsigned output_size;
		unsigned short_freq[4];
		unsigned long_freq[4];
		unsigned literal_bytes;
		unsigned literal_runs;
		unsigned match_bytes;
		unsigned match_count;
		unsigned offset_distance;
		unsigned max_offset;
		unsigned len_freq[259];
		unsigned llen_freq[259];
	} stats;
} lz_context;

// A bit of global configuration data
typedef struct  {
	unsigned bits;
	unsigned base;
	signed limit;
} offset_length_t;

static offset_length_t cfg_short_offset[4];
static offset_length_t cfg_long_offset[4];
#define cfg_short_limit (cfg_short_offset[3].limit)
#define cfg_long_limit (cfg_long_offset[3].limit)

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
	ctx->stats.output_size = ftell(ctx->dst_file);
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

unsigned costof_run_8class(unsigned run) {
	return _log2(run) * 2 + 1;
}

unsigned costof_run(unsigned run) {
	unsigned bits;
	run--;
	if (run == 0) return 1;
	if (run == 1) return 2;
	bits = _log2(run);
	return bits * 2 + 2;
	//return _log2(run) * 2 + 1;
}

unsigned costof_literals_8class(unsigned address, unsigned length) {
        unsigned cost;

        cost = length * 8;
        cost += costof_run_8class(length);

        // A type bit is still always needed after maximum length
        // run since another run may follow
        if(length == RUN_LIMIT) cost++;
        return cost;
}

unsigned costof_literals(unsigned address, unsigned length) {
	unsigned cost;

	cost = length * 8;
	cost += _log2(length) * 2 + 1; //costof_run(length);

	// A type bit is still always needed after maximum length
	// run since another run may follow
	if(length == RUN_LIMIT) cost++;
	return cost;
}

unsigned costof_match_8class(const offset_length_t *class, signed offset, unsigned length) {
        unsigned cost = 3;

        while(offset > class->limit)
                ++class;
        cost += class->bits;

        return cost + costof_run_8class(length - 1);
}

unsigned costof_match(const offset_length_t *class, signed offset, unsigned length) {
	unsigned cost = 3;

	while(offset > class->limit)
		++class;
	cost += class->bits;

	return cost + costof_run(length - 1);
}

lz_info optimal_parsing_literal(lz_context *ctx, const lz_info *info, unsigned cursor) {
	signed length;
	unsigned cost;
	lz_info result;

	length = -info[cursor + 1].match_length;

	if(length > 0 && length < RUN_LIMIT)
		cost = info[cursor + ++length].cumulative_cost;
	else {
		cost = info[cursor + 1].cumulative_cost;
		length = 1;
	}

	if (ctx->output_type == OUTPUT_BITFIRE) {
		cost += costof_literals_8class(cursor, length);
	} else {
		cost += costof_literals(cursor, length);
	}

	result.match_length = -length;
	result.cumulative_cost = cost;

	return result;
}

lz_info optimal_parsing (lz_context *ctx, const lz_info *info, unsigned cursor, signed match_offset, unsigned match_length, unsigned match_limit, lz_info best_match) {
	unsigned cost;

	if(match_length == 2) {
		if(match_offset <= cfg_short_limit) {
			if (ctx->output_type == OUTPUT_BITFIRE) {
				cost = costof_match_8class(cfg_short_offset, match_offset, match_length);
			} else {
				cost = costof_match(cfg_short_offset, match_offset, match_length);
			}
			goto try_short_match;
		} else if(++match_length > match_limit)
			return best_match;
	}

	do {
		if (ctx->output_type == OUTPUT_BITFIRE) {
			cost = costof_match_8class(cfg_long_offset, match_offset, match_length);
		} else {
			cost = costof_match(cfg_long_offset, match_offset, match_length);
		}
try_short_match:
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

	unsigned offset_limit = min(INFINITE_WINDOW, cfg_long_limit);
	unsigned cursor = ctx->src_end;

	info[cursor].cumulative_cost = 0;

	while(cursor != src_begin) {
		unsigned match_length;
		signed cursor_limit;
		unsigned length_limit;
		signed *hash_bucket;
		signed hash_link;
		lz_info best_match;

		--cursor;

		match_length = 1;
		cursor_limit = cursor - offset_limit;

		length_limit = RUN_LIMIT;
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
						best_match
					);

					match_length = i;

					if(match_length == RUN_LIMIT)
						break;
				}
			}

			hash_link = info[hash_link].hash_link;
		}

		info[cursor] = best_match;
	}
	return info[src_begin].cumulative_cost;
}


/******************************************************************************
 * Write the generated matches and literal runs
 ******************************************************************************/

// burps out remaining literals as a plain bnary blob
void encode_literals_plain (lz_context *ctx, unsigned cursor, unsigned length) {
	const unsigned char *data;
	unsigned start = length;

	if(ctx->show_trace) {
		printf ("plain literal(%u bytes)\n",length);
	}

	ctx->stats.literal_bytes += length;
	++ctx->stats.literal_runs;
	++ctx->stats.llen_freq[length];

	data = &ctx->src_data[cursor];
	do
		output_literal(ctx, data[start - length--]);
	while(length);
}

void encode_literals_8class (lz_context *ctx, unsigned cursor, unsigned length) {
	signed bit;
	const unsigned char *data;
	unsigned start = length;

	if(ctx->show_trace) {
		printf ("literal(%u bytes)\n",length);
	}
	ctx->stats.literal_bytes += length;
	++ctx->stats.literal_runs;
	++ctx->stats.llen_freq[length];

	bit = _log2(length);
	while(--bit >= 0) {
		output_bit(ctx, 1);
		output_bit(ctx, length >> bit);
	}

	output_bit(ctx, 0);

	data = &ctx->src_data[cursor];
	do
		output_literal(ctx, data[start - length--]);
	while(length);
}

void encode_literals (lz_context *ctx, unsigned cursor, unsigned length) {
	signed bit;
	const unsigned char *data;
	unsigned start = length;

	ctx->stats.literal_bytes += length;
	++ctx->stats.literal_runs;
	++ctx->stats.llen_freq[length];

	if(ctx->show_trace) printf ("length: ");
	bit = _log2(length);
        while(--bit >= 0) {
                output_bit(ctx, 1);
                output_bit(ctx, length >> bit);
		if (ctx->show_trace) printf(" ");
        }

        output_bit(ctx, 0);

	data = &ctx->src_data[cursor];
	do
		output_literal(ctx, data[start - length--]);
	while(length);
	if (ctx->show_trace) printf("\n");
}

void encode_match_8class (lz_context *ctx, signed offset, unsigned length) {
	unsigned offset_bits;
	unsigned offset_prefix;
	const offset_length_t *offset_class;
	signed length_bit;
#if BITFIRE_WITH_MOFF
	const unsigned prefix_order[] = {0,1,2,3,7,6,5,4};
#else
	const unsigned prefix_order[] = {1,2,0,3,4,5,6,7};
#endif

	if(ctx->show_trace) {
		printf("match(-%u, %u bytes)\n",offset,length);
	}

	++ctx->stats.match_count;
	ctx->stats.match_bytes += length;
	ctx->stats.offset_distance += offset;

	// Write length
	length_bit = _log2(--length) - 1;

	// Write offset prefix
	if(length == 2 - 1) {
		offset_prefix = 0;
		assert(offset <= cfg_short_limit);
		offset_class = cfg_short_offset;

		while(offset > offset_class->limit) {
			++offset_class;
			++offset_prefix;
		}

		offset_prefix = prefix_order[offset_prefix];

		++ctx->stats.short_freq[offset_prefix & 3];
	} else {
		offset_prefix = 4;
		assert(offset <= cfg_long_limit);
		offset_class = cfg_long_offset;

		while(offset > offset_class->limit) {
			++offset_class;
			++offset_prefix;
		}

		offset_prefix = prefix_order[offset_prefix];

		++ctx->stats.long_freq[offset_prefix & 3];
	}

	output_bit(ctx, offset_prefix >> 2);

	while(length_bit >= 0) {
		output_bit(ctx, length >> length_bit);
		output_bit(ctx, --length_bit < 0);
	}

	output_bit(ctx, offset_prefix >> 1);
	output_bit(ctx, offset_prefix >> 0);

	// Write offset payload
	offset -= offset_class->base;

	offset_bits = offset_class->bits;
	if (offset_bits > 7) {
		while(offset_bits & 7) output_bit(ctx,  offset >> --offset_bits);
	} else {
		while(offset_bits & 7) output_bit(ctx, ~offset >> --offset_bits);
	}
	if(offset_bits)	output_literal(ctx, ~offset);
	++ctx->stats.len_freq[length + 2];
}

void encode_match (lz_context *ctx, signed offset, unsigned length) {
	unsigned offset_bits;
	unsigned offset_prefix;
	const offset_length_t *offset_class;
	signed length_bit;
	unsigned init_bit;

	if (offset == 0) length = RUN_LIMIT + 2;
	++ctx->stats.match_count;
	ctx->stats.match_bytes += length;
	ctx->stats.offset_distance += offset;
	if (ctx->stats.max_offset < offset) ctx->stats.max_offset = offset;

	if(ctx->show_trace) printf ("length: ");
	// Write initial length bit
	length_bit = _log2(--length);
	init_bit = --length_bit < 0;
	output_bit(ctx, init_bit);

	length--;

	//nothing to do if length == 1, as the only needed bit is already written
	if (length == 0) {
	} else if (length == 1) {
		//special case, no additional bits will follow after 0 as payload is 0
		output_bit(ctx, 0);
	//now handle lengths > 2
	} else {
		output_bit(ctx, 1);
		length_bit = _log2(length);
	        while(--length_bit >= 0) {
			if (ctx->show_trace) printf(" ");
	                output_bit(ctx, length >> length_bit);
			//if (ctx->show_trace) printf(" ");
			if (length >= 255) {
				//skip stop bit if number has 8 bits
				if (length_bit != 0) {
					output_bit(ctx, length_bit != 0);
				} else {
					if (ctx->show_trace) printf("s");
				}
			} else {
				output_bit(ctx, length_bit != 0);
			}
	        }
	}
	if (ctx->show_trace) printf("\n");

	if (length >= RUN_LIMIT) return;

	// Write offset prefix
	if(length == 0) {
		assert(offset <= cfg_short_limit);
		offset_prefix = 0;
		offset_class = cfg_short_offset;

		while(offset > offset_class->limit) {
			++offset_class;
			++offset_prefix;
		}

		++ctx->stats.short_freq[offset_prefix];
		++ctx->stats.len_freq[length + 2];

		// Note that the encoding for short matches is reversed relative to
		// the long ones in order to expose holes in the decruncher's offset
		// tables
		offset_prefix = ~offset_prefix;
	} else {
		assert(offset <= cfg_long_limit);
		offset_prefix = 0;
		offset_class = cfg_long_offset;

		while(offset > offset_class->limit) {
			++offset_class;
			++offset_prefix;
		}

		++ctx->stats.long_freq[offset_prefix];
		++ctx->stats.len_freq[length + 2];
	}

	if(ctx->show_trace) printf("offset: (%d)", init_bit);
	output_bit(ctx, offset_prefix >> 1);
	output_bit(ctx, offset_prefix >> 0);
	if(ctx->show_trace) printf("-");

	// Write offset payload
	offset -= offset_class->base;
	offset = ~offset;

	offset_bits = offset_class->bits;
	while(offset_bits & 7)
		output_bit(ctx, offset >> --offset_bits);

	if(offset_bits) {
		output_literal(ctx, offset);
		if (ctx->show_trace) printf("$%02x", offset & 0xff);
	}

	if (ctx->show_trace) printf("\n");
}

void render_output(lz_context *ctx) {
	unsigned cursor;

	bool implicit_match = false;

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

			if(!implicit_match) {
				if(ctx->show_trace) printf ("type_bit ");
				if(update) output_bit(ctx, 0);
				if(ctx->show_trace) printf ("\n");
			}

			if (ctx->output_type == OUTPUT_BITFIRE) {
				if (!update) {
					encode_literals_plain(ctx, cursor, length);
				} else {
					encode_match_8class(ctx, offset, length);
				}
			} else {
				encode_match(ctx, offset, length);
			}
			if(ctx->show_trace) printf ("\n");

			implicit_match = false;
			last_match = cursor + length;
		} else {
			sentinel_needed = true;
			length = -length;

			if(ctx->show_trace) printf ("type_bit ");
			if(update) output_bit(ctx, 1);
			if(ctx->show_trace) printf ("\n");

			// Normally a match implicitly follows a literal run except for the
			// case of a maximum length literal run
			implicit_match = length < RUN_LIMIT;

			// The parser may generate a short run followed by one or more maximum
			// length runs for split literals. This needs to be avoided manually
			// by reversing the order
			if(implicit_match) {
				signed next_length = -info[cursor + length].match_length;
				//is the next run a literal too? is the length of the current literal smaller then the run limit and is the next element a literal of maximum runlength? If so, swap both, and if things continue, do so also on upcoming turns.
				if(next_length > 0 && next_length == RUN_LIMIT && length < RUN_LIMIT) {
				//if(next_length > 0) {
					info[cursor].match_length = -next_length;
					info[cursor + next_length].match_length = -length;

					//check that first element is < RUN_LIMIT if we swap elements
					assert(length < RUN_LIMIT);

					length = next_length;
					implicit_match = false;
				}
			}

			if (ctx->output_type == OUTPUT_BITFIRE) {
				if (!update) {
					encode_literals_plain(ctx, cursor, length);
				} else {
					encode_literals_8class(ctx, cursor, length);
				}
			}
			else {
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

		if(expected != actual) {
			printf (
				"expected: %u\n"
				"actual:   %u\n",
				expected,
				actual
			);
		}
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
			if(!implicit_match) {
				if(ctx->show_trace) printf ("type_bit ");
				output_bit(ctx, 0);
				if(ctx->show_trace) printf ("\n");
			}

//			printf("%d bits saved\n", saved);

			if (ctx->output_type == OUTPUT_BITFIRE) {
				length_bit = _log2(RUN_LIMIT);
				output_bit(ctx, --length_bit >= 0);

				while(length_bit >= 0) {
					output_bit(ctx, RUN_LIMIT >> length_bit);
					output_bit(ctx, --length_bit < 0);
				}
			} else {
				encode_match(ctx, 1, RUN_LIMIT+3);
			}
		}
	}

	return;
}


/******************************************************************************
 * Parse out the set of offset bit lengths from a descriptor string
 ******************************************************************************/

//XXX TODO basically the same, except table->base = 0 and base is unused?
static void prepare_offset_lengths_8class(offset_length_t *table, size_t count) {
	unsigned limit = 0;
	unsigned previous = 0;

	do {
		unsigned int bits = table->bits;

		if(bits <= previous)
			fatal("offset lengths must be listed in ascending order");
		previous = bits;
		if(bits > OFFSET_LENGTH_LIMIT)
			fatal("offset lengths cannot be wider than %u bits", OFFSET_LENGTH_LIMIT);

		limit = 1 << bits;
		table->base = 1;
		table->limit = limit;
		++table;
	} while(--count);
}

static void prepare_offset_lengths(offset_length_t *table, size_t count) {
	unsigned base;
	unsigned limit = 0;
	unsigned previous = 0;

	do {
		unsigned int bits = table->bits;

		if(bits <= previous)
			fatal("offset lengths must be listed in ascending order");
		previous = bits;
		if(bits > OFFSET_LENGTH_LIMIT)
			fatal("offset lengths cannot be wider than %u bits", OFFSET_LENGTH_LIMIT);

		base = limit + 1;
		limit += 1 << bits;
		table->base = base;
		table->limit = limit;
		++table;
	} while(--count);
}

bool parse_offset_lengths(lz_context* ctx, const char *text) {
	if(sscanf(text, "%u/%u/%u/%u:%u/%u/%u/%u",
		&cfg_short_offset[0].bits, &cfg_short_offset[1].bits,
		&cfg_short_offset[2].bits, &cfg_short_offset[3].bits,
		&cfg_long_offset[0].bits, &cfg_long_offset[1].bits,
		&cfg_long_offset[2].bits, &cfg_long_offset[3].bits) != 8) {
		return false;
	}
	if (ctx->output_type == OUTPUT_BITFIRE && !BITFIRE_WITH_MOFF) {
		prepare_offset_lengths_8class(cfg_short_offset, 4);
		prepare_offset_lengths_8class(cfg_long_offset, 4);
	} else {
		prepare_offset_lengths(cfg_short_offset, 4);
		prepare_offset_lengths(cfg_long_offset, 4);
	}
	return true;
}


/******************************************************************************
 * Generate decruncher the tables corresponding to a particular offset length
 * sequence.
 * These are admittedly rather convoluted and tied tightly to how matches are
 * handled in the implementation. See the source for details
 ******************************************************************************/

static char single_offset (const offset_length_t *class, unsigned shift) {
	signed offset = class->base + 1;
	if(class->bits > 8)
		offset += 0x8000;
	else if(class->bits == 8)
		offset += 0x0100;
	offset = -offset;
	offset >>= shift;
	offset &= 0xFF;

	return offset;
}

static void write_single_offset (FILE *file, const offset_length_t *class, unsigned shift) {
	char binary[9];

	signed offset = class->base + 1;
	if(class->bits > 8)
		offset += 0x8000;
	else if(class->bits == 8)
		offset += 0x0100;
	offset = -offset;
	offset >>= shift;
	offset &= 0xFF;

	{
		char *digit = &binary[8];
		*digit = '\0';
		do {
			*--digit = (offset & 1)["01"];
			offset >>= 1;
		} while(digit != binary);
	}

	fprintf (
		file,
		"%s\t\t.byte %%%s\t\t;%u-%u%s\n",
		class->bits < shift ? ";" : "",
		binary,
		class->base,
		class->limit,
		class->bits < shift ? " (unreferenced)" : ""
	);
}

void write_offsets(FILE* file) {
	static const char const length_codes[] = {
		0,
		0xff,
		0x7f,
		0x3f,
		0x1f,
		0x0f,
		0x07,
		0x03,
		0,
		0xbf,
		0x5f,
		0x2f,
		0x17,
		0x0b,
		0x05,
		0x02
	};

	ptrdiff_t i;

	for(i = 0; i <= 3; ++i) {
		unsigned bits = cfg_long_offset[i].bits;
		fputc(length_codes[bits], file);
	}
	for(i = 3; i >= 0; --i) {
		unsigned bits = cfg_short_offset[i].bits;
		fputc(length_codes[bits], file);
	}
	for(i = 0; i <= 3; ++i)
		fputc(single_offset(&cfg_long_offset[i], 0), file);
	for(i = 3; i >= 0; --i)
		fputc(single_offset(&cfg_short_offset[i], 0), file);
	// MSB of base offsets
	for(i = 0; i <= 3; ++i)
		fputc(single_offset(&cfg_long_offset[i], 8), file);
	for(i = 3; i >= 0; --i)
		fputc(single_offset(&cfg_short_offset[i], 8), file);
}

void write_offset_tables(const char *name) {
	static const char long_title[] = "\t\t;Long (>2 byte matches)\n";
	static const char short_title[] = "\t\t;Short (2 byte matches)\n";

	static const char *const length_codes[] = {
		NULL,
		"11111111", // 1-bits
		"01111111", // 2-bits
		"00111111", // 3-bits
		"00011111", // 4-bits
		"00001111", // 5-bits
		"00000111", // 6-bits
		"00000011", // 7-bits
		"00000000", // 8-bits: This needs a bit of special-processing
		"10111111", // 9-bits
		"01011111", // 10-bits
		"00101111", // 11-bits
		"00010111", // 12-bits
		"00001011", // 13-bits
		"00000101", // 14-bits
		"00000010"  // 15-bits
	};

	ptrdiff_t i;
	unsigned near_longs;

	// Open the target file
	FILE *file = fopen(name, "w");
	if(!file)
		fatal("cannot create '%s'", name);

	// Bit lengths
	fprintf(file, "_lz_moff_length\n");
	fprintf(file, long_title);
	near_longs = 0;
	for(i = 0; i <= 3; ++i) {
		unsigned bits = cfg_long_offset[i].bits;
		fprintf(file, "\t\t.byte %%%s\t\t;%u bits\n",
			length_codes[bits], bits);
		if(bits < 8)
			++near_longs;
	}
	fprintf(file, short_title);
	for(i = 3; i >= 0; --i) {
		unsigned bits = cfg_short_offset[i].bits;
		fprintf(file, "\t\t.byte %%%s\t\t;%u bits\n",
			length_codes[bits], bits);
	}
	// LSB of base offsets
	fprintf(file, "_lz_moff_adjust_lo\n");
	fprintf(file, long_title);
	for(i = 0; i <= 3; ++i)
		write_single_offset(file, &cfg_long_offset[i], 0);
	fprintf(file, short_title);
	for(i = 3; i >= 0; --i)
		write_single_offset(file, &cfg_short_offset[i], 0);
	// MSB of base offsets
	fprintf(file, "_lz_moff_adjust_hi = *-%u\n", near_longs);
	fprintf(file, long_title);
	for(i = 0; i <= 3; ++i)
		write_single_offset(file, &cfg_long_offset[i], 8);
	fprintf(file, short_title);
	for(i = 3; i >= 0; --i)
		write_single_offset(file, &cfg_short_offset[i], 8);

	fclose(file);
}


/******************************************************************************
 * Print some basic statistics about the encoding of the file
 ******************************************************************************/
void print_statistics(const lz_context *ctx, FILE *file) {
	unsigned input_size = ctx->src_end - ctx->src_begin;
	unsigned i;

	printf (
		"input file:\t"    "%u bytes\n"
		"output file:\t"   "%u bytes, %u bits (%.2f%% ratio)\n"
		"short offsets:\t" "{ %u-%u: %u, %u-%u: %u, %u-%u: %u, %u-%u: %u }\n"
		"long offsets:\t"  "{ %u-%u: %u, %u-%u: %u, %u-%u: %u, %u-%u: %u }\n"
		"%u matches:\t"    "%u bytes, %f avg, %d max\n"
		"%u literals:\t"   "%u bytes, %f avg\n"
		"avg offset:\t"    "%f bytes\n",

		input_size,
		ctx->stats.output_size,
		ctx->info[ctx->src_begin].cumulative_cost,
		100.0 * ctx->stats.output_size / input_size,

		cfg_short_offset[0].base,
		cfg_short_offset[0].limit,
		ctx->stats.short_freq[0],
		cfg_short_offset[1].base,
		cfg_short_offset[1].limit,
		ctx->stats.short_freq[1],
		cfg_short_offset[2].base,
		cfg_short_offset[2].limit,
		ctx->stats.short_freq[2],
		cfg_short_offset[3].base,
		cfg_short_offset[3].limit,
		ctx->stats.short_freq[3],
		cfg_long_offset[0].base,
		cfg_long_offset[0].limit,
		ctx->stats.long_freq[0],
		cfg_long_offset[1].base,
		cfg_long_offset[1].limit,
		ctx->stats.long_freq[1],
		cfg_long_offset[2].base,
		cfg_long_offset[2].limit,
		ctx->stats.long_freq[2],
		cfg_long_offset[3].base,
		cfg_long_offset[3].limit,
		ctx->stats.long_freq[3],

		ctx->stats.match_count,
		ctx->stats.match_bytes,
		(double) ctx->stats.match_bytes / ctx->stats.match_count,
		ctx->stats.max_offset,

		ctx->stats.literal_runs,
		ctx->stats.literal_bytes,
		(double) ctx->stats.literal_bytes / ctx->stats.literal_runs,

		(double) ctx->stats.offset_distance / ctx->stats.match_count
	);
	for (i = 0; i < 256; i++) {
		printf("% 6d   % 6d\n", ctx->stats.llen_freq[i], ctx->stats.len_freq[i]);
	}
}

/******************************************************************************
 * Helper functions
 ******************************************************************************/
signed read_number(char* arg) {
	if(arg[0] == '$') return strtoul(arg + 1, NULL, 16);
	else if(arg[0] == '0' && arg[1] == 'x') return strtoul(arg + 2, NULL, 16);
	return strtoul(arg, NULL, 10);
}

unsigned compress(lz_context* ctx, char* output_name) {
	generate_hash_table(ctx);
	return find_matches(ctx);
}

/******************************************************************************
 * 
 ******************************************************************************/

void iterate(lz_context* ctx) {
	unsigned packed_size;

	unsigned temp;
	signed j;
	unsigned smin, smax;
	unsigned lmin, lmax;
	unsigned from, to;

	char lengths[24];
	unsigned lbits[6], sbits[6];
	unsigned lbest, sbest;

	lbits[0] = 3;
	lbits[1] = 7;
	lbits[2] = 10;
	lbits[3] = 13;

	setbuf(stdout, NULL);

	packed_size = 0;
	smin = 1;
	smax = 15;

	lmin = 1;
	lmax = 15;

	ctx->iterations = 1;
	while (ctx->iterations--) {
		for (j = smin; j <= smax - 3; j++) {
			sbits[0] = j;
			sbits[3] = smax;
			sbits[1] = j + (smax - j) / 3;
			sbits[2] = j + (smax - j) / 3 * 2;
			sprintf(lengths,"%u/%u/%u/%u:%u/%u/%u/%u",sbits[0],sbits[1],sbits[2],sbits[3],lbits[0],lbits[1],lbits[2],lbits[3]);
			parse_offset_lengths(ctx, lengths);
			temp = compress(ctx, ctx->output_name);
			if (packed_size == 0 || temp < packed_size) {
				printf("best bitlengths: %s        \r", lengths);
				smin = j; packed_size = temp;
			}
		}
		for (j = smax; j > smin + 3; j--) {
			sbits[0] = smin;
			sbits[3] = j;
			sbits[1] = smin + (j - smin) / 3;
			sbits[2] = smin + (j - smin) / 3 * 2;
			sprintf(lengths,"%u/%u/%u/%u:%u/%u/%u/%u",sbits[0],sbits[1],sbits[2],sbits[3],lbits[0],lbits[1],lbits[2],lbits[3]);
			parse_offset_lengths(ctx, lengths);
			temp = compress(ctx, ctx->output_name);
			if (packed_size == 0 || temp < packed_size) {
				printf("best bitlengths: %s        \r", lengths);
				smax = j; packed_size = temp;
			}

		}
		sbits[0] = smin;
		sbits[3] = smax;
		sbits[1] = smin + (smax - smin) / 3;
		sbits[2] = smin + (smax - smin) / 3 * 2;

		for (j = lmin; j <= lmax - 3; j++) {
			lbits[0] = j;
			lbits[3] = lmax;
			lbits[1] = j + (lmax - j) / 3;
			lbits[2] = j + (lmax - j) / 3 * 2;
			sprintf(lengths,"%u/%u/%u/%u:%u/%u/%u/%u",sbits[0],sbits[1],sbits[2],sbits[3],lbits[0],lbits[1],lbits[2],lbits[3]);
			parse_offset_lengths(ctx, lengths);
			temp = compress(ctx, ctx->output_name);
			if (packed_size == 0 || temp < packed_size) {
				printf("best bitlengths: %s        \r", lengths);
				lmin = j; packed_size = temp;
			}
		}
		for (j = lmax; j > lmin + 3; j--) {
			lbits[0] = lmin;
			lbits[3] = j;
			lbits[1] = lmin + (j - lmin) / 3;
			lbits[2] = lmin + (j - lmin) / 3 * 2;
			sprintf(lengths,"%u/%u/%u/%u:%u/%u/%u/%u",sbits[0],sbits[1],sbits[2],sbits[3],lbits[0],lbits[1],lbits[2],lbits[3]);
			parse_offset_lengths(ctx, lengths);
			temp = compress(ctx, ctx->output_name);
			if (packed_size == 0 || temp < packed_size) {
				printf("best bitlengths: %s        \r", lengths);
				lmax = j; packed_size = temp;
			}

		}
		lbits[0] = lmin;
		lbits[3] = lmax;
		lbits[1] = lmin + (lmax - lmin) / 3;
		lbits[2] = lmin + (lmax - lmin) / 3 * 2;


	}

	packed_size = 0;
	ctx->iterations = 2;

	while (ctx->iterations--) {

                for (j = 0; j < 4; j++) {
                        lbest = lbits[j];
			if (j == 0) from = 1;
			else from = lbits[j-1] + 1;

			if (j == 3) to = 16;
			else to = lbits[j+1] - 1;

                        for (lbits[j] = from; lbits[j] < to; lbits[j]++) {
                                sprintf(lengths,"%u/%u/%u/%u:%u/%u/%u/%u",sbits[0],sbits[1],sbits[2],sbits[3],lbits[0],lbits[1],lbits[2],lbits[3]);
                                parse_offset_lengths(ctx, lengths);
                                temp = compress(ctx, ctx->output_name);
                                if (packed_size == 0 || temp < packed_size) {
                                        printf("best bitlengths: %s        \r", lengths);
                                        lbest = lbits[j]; packed_size = temp;
                                }
                        }
                       	lbits[j] = lbest;
                }
                for (j = 0; j < 4; j++) {
                        sbest = sbits[j];
			if (j == 0) from = 1;
			else from = sbits[j-1] + 1;

			if (j == 3) to = 16;
			else to = sbits[j+1] - 1;

                        for (sbits[j] = from; sbits[j] < to; sbits[j]++) {
                                sprintf(lengths,"%u/%u/%u/%u:%u/%u/%u/%u",sbits[0],sbits[1],sbits[2],sbits[3],lbits[0],lbits[1],lbits[2],lbits[3]);
                                parse_offset_lengths(ctx, lengths);
                                temp = compress(ctx, ctx->output_name);
                                if (packed_size == 0 || temp < packed_size) {
                                        printf("best bitlengths: %s        \r", lengths);
                                        sbest = sbits[j]; packed_size = temp;
                                }
                        }
                       	sbits[j] = sbest;
		}
	}

	sprintf(lengths,"%u/%u/%u/%u:%u/%u/%u/%u",sbits[0],sbits[1],sbits[2],sbits[3],lbits[0],lbits[1],lbits[2],lbits[3]);
	parse_offset_lengths(ctx, lengths);
	printf("best bitlengths: %s        \n", lengths);
}

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

	if (ctx->iterations > 0) iterate(ctx);

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

	if(ctx->write_tables) write_offsets(ctx->dst_file);

	// avoid to enable depack_to mechanisms when src-addr is already depack-destination
	if(ctx->depack_to == ctx->src_begin) {
		ctx->depack_to = -1;
	}

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
		printf("overlap: %d bytes\n", ctx->margin);
	}

	// Some stats and info
	printf("source size: $%04x (%d)\n", source_size, source_size);
	printf("packed size: $%04x (%d) %s ratio: %.1f%%\n", packed_size - ctx->write_tables * 24, packed_size - ctx->write_tables * 24, ctx->write_tables ? "(+24 byte tables)" : "", ((packed_size - ctx->write_tables * 24) * 100.0 / (ctx->output_end - ctx->output_begin)));

	// Print more info and calc addresses
	if(ctx->output_type == OUTPUT_LEVEL || ctx->output_type == OUTPUT_BITFIRE) {
		//find perfect loading address
		if (ctx->load_addr < 0) ctx->load_addr = ctx->output_end - packed_size + ctx->margin;
		if (ctx->relocate_to >= 0 && ctx->load_addr >= 0) {
			ctx->load_addr = ctx->load_addr + (ctx->relocate_to - ctx->output_begin);
			ctx->end_pos = ctx->end_pos + (ctx->relocate_to - ctx->output_begin);
		}

		printf("source load: $%04x-$%04x\n", ctx->output_begin, ctx->output_end);
		printf("packed load: $%04x-$%04x\n", ctx->load_addr, ctx->load_addr + packed_size);

		// Fix optimal load address to file
		fseek(ctx->dst_file, 0, SEEK_SET);
		fputc(ctx->load_addr & 0xff, ctx->dst_file);
		fputc(ctx->load_addr >> 8, ctx->dst_file);
		// Add depack address
		if (ctx->depack_to >=0) {
			fputc(ctx->depack_to & 0xff, ctx->dst_file);
			fputc(ctx->depack_to >> 8, ctx->dst_file);
		} else {
			if (ctx->relocate_to >= 0) {
				fputc(ctx->relocate_to & 0xff, ctx->dst_file);
				fputc(ctx->relocate_to >> 8, ctx->dst_file);
			} else {
				fputc(ctx->output_begin & 0xff, ctx->dst_file);
				fputc(ctx->output_begin >> 8, ctx->dst_file);
			}
		}
		if (!ctx->overlap) {
			// Add end address
			fputc(ctx->end_pos & 0xff, ctx->dst_file);
			fputc(ctx->end_pos >> 8, ctx->dst_file);
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
			printf("filetype: bitfire\n");
		} else {
			printf("filetype: level\n");
		}
	} else if(ctx->output_type == OUTPUT_SFX) {
		packed_size -= 26;
		data_addr = decruncher_size + packed_size - 0xff + 0x800 + 24;

		// Set up depacker values
		fseek(ctx->dst_file, 0x2e, SEEK_SET);
		fputc((packed_size >> 8) + 1, ctx->dst_file);

		fseek(ctx->dst_file, 0x31, SEEK_SET);
		fputc(data_addr & 0xff, ctx->dst_file);
		fputc(data_addr >> 8, ctx->dst_file);

		fseek(ctx->dst_file, 0x43, SEEK_SET);
		fputc((0x10000 - packed_size) & 0xff, ctx->dst_file);

		fseek(ctx->dst_file, 0x6a, SEEK_SET);
		fputc((0x10000 - packed_size) >> 8, ctx->dst_file);
		fseek(ctx->dst_file, 0xd3, SEEK_SET);
		fputc((0x10000 - packed_size) >> 8, ctx->dst_file);
		fseek(ctx->dst_file, 0x118, SEEK_SET);
		fputc((0x10000 - packed_size) >> 8, ctx->dst_file);

		fseek(ctx->dst_file, 0x72, SEEK_SET);
		fputc(ctx->output_begin & 0xff, ctx->dst_file);
		fputc(ctx->output_begin >> 8, ctx->dst_file);

		printf("start address: $%04x (%d)\n", ctx->start_addr, ctx->start_addr);
		printf("final size: $%04x (%d)\n", packed_size + decruncher_size, packed_size + decruncher_size);
		printf("filetype: sfx\n");
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
	if(ctx->show_stats) print_statistics(ctx, stdout);
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

	memset(&ctx.stats, 0, sizeof ctx.stats);
	// Parse the command line
	program_name = *argv;

	ctx.iterations = 0;
	ctx.output_name = NULL;
	ctx.cut_addr_first = 0;
	ctx.cut_addr_last = INT_MAX;
	ctx.emit_offset_tables = NULL;
	ctx.show_stats = false;

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
	ctx.write_tables = false;
	ctx.offset_lengths = false;

	while(++argv, --argc) {
		if(argc >= 2 && !strcmp(*argv, "-o")) {
			ctx.output_name = *++argv;
			--argc;
		} else if(argc >= 2 && !strcmp(*argv, "--sfx")) {
			ctx.start_addr = read_number(*++argv);
			ctx.output_type = OUTPUT_SFX;
			ctx.write_tables = true;
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
		} else if(!strcmp(*argv, "--overlap")) {
			ctx.overlap = true;
		} else if(!strcmp(*argv, "--best-offset-tables")) {
			ctx.iterations = 1;
		} else if(argc >= 2 && !strcmp(*argv, "--offset-lengths")) {
			ctx.offset_lengths = true;
			if(!parse_offset_lengths(&ctx, *++argv))
				break;
			--argc;
		} else if(argc >= 2 && !strcmp(*argv, "--emit-offset-tables")) {
			ctx.emit_offset_tables = *++argv;
			--argc;
		} else if(!strcmp(*argv, "--include-tables")) {
			ctx.write_tables = true;
		} else if(!strcmp(*argv, "--statistics")) {
			ctx.show_stats = true;
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

	if (!ctx.offset_lengths) parse_offset_lengths(&ctx, DEFAULT_LENGTHS);

	if(ctx.emit_offset_tables) {
		write_offset_tables(ctx.emit_offset_tables);
		// It allowed to just generate the offset tables
		if(!argc)
			return EXIT_SUCCESS;
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
			"\t[--best-offset-tables]                               evaluate best offset tables, slow\n"
			"\t[--include-tables]                                   add own tables to packed sfx\n"
			"\t[--offset-lengths s1/s2/s3/s4:l1/l2/l3/l4]           use alternative offset lengths\n"
			"\t[--emit-offset-tables tables.asm]                    spit out offset table\n"
			"\t[--statistics]                                       print some stats\n"
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

	if (ctx.output_type == OUTPUT_BITFIRE) {
		if (ctx.offset_lengths) {
			fatal("option --offset_lengths not supported with --bitfire");
		}
		if (ctx.write_tables) {
			fatal("option --write_tables not supported with --bitfire");
		}
		if (ctx.iterations > 0) {
			fatal("option --best_offset_tables not supported with --bitfire");
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
