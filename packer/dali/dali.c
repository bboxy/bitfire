#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "sfx.h"

#define BUFFER_SIZE 65536  /* must be > MAX_OFFSET */
#define INITIAL_OFFSET 1

#define FALSE 0
#define TRUE 1

typedef struct ctx {
    FILE *packed_fp;
    FILE *reencoded_fp;
    FILE *unpacked_fp;
    FILE *clamped_fp;
    unsigned char *packed_data;
    unsigned char *reencoded_data;
    unsigned char *unpacked_data;
    size_t packed_index;
    size_t packed_size;
    int packed_bit_mask;
    int packed_bit_value;
    size_t reencoded_index;
    int reencoded_bit_mask;
    int reencoded_bit_value;
    int reencoded_bit_index;
    size_t unpacked_index;
    size_t unpacked_size;
    int inplace;
} ctx;

void salvador_main();

static int read_number(char* arg, int limit) {
    int number;
    if(arg[0] == '$') number = strtoul(arg + 1, NULL, 16);
    else if(arg[0] == '0' && arg[1] == 'x') number = strtoul(arg + 2, NULL, 16);
    else number = strtoul(arg, NULL, 10);
    if (number < 0 || number > limit) {
        fprintf(stderr, "Error: Number '%s' out of range (0 - 65536)\n", arg);
        exit(1);
    }
    return number;
}

static void file_write_byte(int byte, FILE *ofp) {
    if (fputc(byte, ofp) != byte) {
        fprintf(stderr, "Error: Cannot write output file\n");
        perror("fputc");
        exit(1);
    }
    return;
}

static inline unsigned bit_size(unsigned value) {
#   ifdef __GNUC__
//    enum { WORD_BITS = sizeof(unsigned) * CHAR_BIT };

    return ((sizeof(unsigned) * 8 - 1) ^ __builtin_clz(value));
#   else
    signed bits = -1;

    do
        ++bits;
    while(value >>= 1);

    return bits;
#   endif
}

void write_reencoded_byte(ctx* ctx, int value) {
    ctx->reencoded_data[ctx->reencoded_index++] = value;
}

void write_reencoded_bit(ctx* ctx, int value) {
    if (!(ctx->reencoded_bit_mask & 255)) {
        ctx->reencoded_bit_mask = 0x80;
        /* remember position of bit-buffer */
        ctx->reencoded_bit_index = ctx->reencoded_index;
        write_reencoded_byte(ctx, 0);
    }
    if (value)
        ctx->reencoded_data[ctx->reencoded_bit_index] |= ctx->reencoded_bit_mask;
    ctx->reencoded_bit_mask >>= 1;
}

void write_reencoded_interlaced_elias_gamma(ctx* ctx, int value, int skip) {
    int bits = bit_size(value);
    int i;

    for (i = 2; i <= value; i <<= 1);
    i >>= 1;

    if (bits >= 8) {
        /* change bit-order, send LSB first */
        /* remove preceeding 1 first */
        value = value & ((0xffff ^ i));
        /* move LSB bits to the beginning */
        value = (value >> 8) | ((value & 0xff) << (bits - 8));
    }

    while ((i >>= 1) > 0) {
        if (!skip) write_reencoded_bit(ctx, 0);
        skip = 0;
        write_reencoded_bit(ctx, ((value & i) > 0));
    }
    if (!skip) write_reencoded_bit(ctx, 1);
}

int read_byte(ctx* ctx) {
    return ctx->packed_data[ctx->packed_index++];
}

int read_bit(ctx* ctx) {
    if ((ctx->packed_bit_mask >>= 1) == 0) {
        ctx->packed_bit_mask = 0x80;
        ctx->packed_bit_value = read_byte(ctx);
    }
    return (ctx->packed_bit_value & ctx->packed_bit_mask) != 0;
}

int read_interlaced_elias_gamma(ctx* ctx, int inverted, int skip) {
    int value = 1;
    /* skip first read bit if skip != 0 */
    while (skip || !read_bit(ctx)) {
        skip = 0;
        value = (value << 1) | (read_bit(ctx) ^ inverted);
    }
    return value;
}

void save_reencoded(ctx* ctx, int cbm_orig_addr, int cbm_packed_addr) {
    if (ctx->packed_index != 0) {
        /* little endian */
        file_write_byte(cbm_packed_addr & 255, ctx->reencoded_fp);
        file_write_byte((cbm_packed_addr >> 8) & 255, ctx->reencoded_fp);
        /* big endian, as read backwards by depacker */
        file_write_byte((cbm_orig_addr >> 8) & 255, ctx->reencoded_fp);
        file_write_byte(cbm_orig_addr & 255, ctx->reencoded_fp);

        if (fwrite(ctx->reencoded_data, sizeof(char), ctx->packed_index, ctx->reencoded_fp) != ctx->packed_index) {
            fprintf(stderr, "Error: Cannot write output file\n");
            perror("fwrite");
            exit(1);
        }
    }
}

void copy_inplace_literal(ctx* ctx) {
    int i;
    for (i = ctx->unpacked_index; i < ctx->unpacked_size; i++) {
        ctx->reencoded_data[ctx->packed_index] = ctx->unpacked_data[i];
        ctx->packed_index++;
    }
}

void encode_literal(ctx* ctx, int length, int first) {
    int i;
    if (!first) write_reencoded_bit(ctx, 0);
    write_reencoded_interlaced_elias_gamma(ctx, length, 0);
    for (i = 0; i < length; i++) {
        write_reencoded_byte(ctx, ctx->unpacked_data[ctx->unpacked_index + i]);
    }
}

void encode_rep(ctx* ctx, int length) {
    write_reencoded_bit(ctx, 0);
    write_reencoded_interlaced_elias_gamma(ctx, length, 0);
}

void encode_match(ctx* ctx, int length, int offset) {
    write_reencoded_bit(ctx, 1);
    write_reencoded_interlaced_elias_gamma(ctx, ((offset - 1) >> 7) + 1, 0);
    write_reencoded_byte(ctx, (((offset - 1) & 0x7f) << 1) | (length == 2));
    write_reencoded_interlaced_elias_gamma(ctx, length - 1, 1);
}

void reencode(ctx* ctx) {
    int last_offset = INITIAL_OFFSET;
    int length;
    int overwrite;

    int bit, byte;
    int i;

    int safe_input_index = 0;
    int safe_output_index = 0;

    int first = 1;

    ctx->packed_index = 0;
    ctx->packed_bit_mask = 0;

    ctx->reencoded_index = 0;
    ctx->reencoded_bit_mask = 0;
    ctx->reencoded_bit_value = 0;
    ctx->reencoded_bit_index = 0;

    ctx->packed_index = 0;
    ctx->unpacked_index = 0;

    ctx->packed_bit_mask = 0;

    while (1) {
        /* literal */
        length = read_interlaced_elias_gamma(ctx, FALSE, 0);
        for (i = 0; i < length; i++) read_byte(ctx);
        encode_literal(ctx, length, first);
        first = 0;

        ctx->unpacked_index += length;

        overwrite = (ctx->unpacked_index) - (ctx->unpacked_size - ctx->packed_size + ctx->packed_index);
        /* literal would overwrite packed src */
        if (ctx->inplace && overwrite >= 0) {
            /* go back to previous index */
            ctx->unpacked_index = safe_input_index;
            ctx->packed_index = safe_output_index;
            copy_inplace_literal(ctx);
            return;
        }
        /* do remember last safe position */
        safe_input_index = ctx->unpacked_index;
        safe_output_index = ctx->packed_index;

        /* copy from new or last offset? */
        bit = read_bit(ctx);
        if (!bit) {
            /* copy from last_offset */
            length = read_interlaced_elias_gamma(ctx, FALSE, 0);
            encode_rep(ctx, length);

            ctx->unpacked_index += length;

            overwrite = (ctx->unpacked_index) - (ctx->unpacked_size - ctx->packed_size + ctx->packed_index);
            /* rep would overwrite packed src */
            if (ctx->inplace && overwrite >= 0) {
                copy_inplace_literal(ctx);
                return;
            }
            safe_input_index = ctx->unpacked_index;
            safe_output_index = ctx->packed_index;

            bit = read_bit(ctx);
        }

        while (bit) {
            /* copy from new_offset */
            last_offset = read_interlaced_elias_gamma(ctx, TRUE, 0);
            if (last_offset == 256) {
                if (!ctx->inplace) {
                    write_reencoded_bit(ctx, 1);
                    write_reencoded_interlaced_elias_gamma(ctx, last_offset, 0);
                }
                return;
            }
            byte = read_byte(ctx);
            if (byte & 1) length = 2;
            else length = read_interlaced_elias_gamma(ctx, FALSE, 1) + 1;
            last_offset = (last_offset << 7) - (byte >> 1);
            encode_match(ctx, length, last_offset);

            ctx->unpacked_index += length;

            overwrite = (ctx->unpacked_index) - (ctx->unpacked_size - ctx->packed_size + ctx->packed_index);
            /* rep would overwrite packed src */
            if (ctx->inplace && overwrite >= 0) {
                copy_inplace_literal(ctx);
                return;
            }
            safe_input_index = ctx->unpacked_index;
            safe_output_index = ctx->packed_index;

            bit = read_bit(ctx);
        }
    }
}

int main(int argc, char *argv[]) {
    int cbm_orig_addr = 0;
    int cbm_packed_addr = 0;
    int cbm_range_from = -1;
    int cbm_range_to = -1;
    int cbm_relocate_packed_addr = -1;
    int cbm_relocate_origin_addr = -1;

    int cbm;

    int sfx;
    int sfx_addr = -1;
    int sfx_01 = 0x37;
    char *sfx_code = NULL;

    char *output_name = NULL;
    char *input_name = NULL;
    //char *compressor_path = NULL;
    //char *shell_call = NULL;

    int i;

    char *salvador_argv[3];

    ctx ctx = { 0 };

    ctx.inplace = TRUE;
    cbm = TRUE;
    sfx = FALSE;

    for (i = 1; i < argc; i++) {
        if (!strncmp(argv[i], "-", 1) || !strncmp(argv[i], "--", 2)) {
            if (!strcmp(argv[i], "--binfile")) {
                cbm = FALSE;
            } else if (!strcmp(argv[i], "--no-inplace")) {
                ctx.inplace = FALSE;
            } else if (!strcmp(argv[i], "--relocate-packed")) {
                i++;
                cbm_relocate_packed_addr = read_number(argv[i], 65536);
            } else if (!strcmp(argv[i], "--relocate-origin")) {
                i++;
                cbm_relocate_origin_addr = read_number(argv[i], 65536);
            } else if (!strcmp(argv[i], "--from")) {
                i++;
                cbm_range_from = read_number(argv[i], 65536);
            } else if (!strcmp(argv[i], "--to")) {
                i++;
                cbm_range_to = read_number(argv[i], 65536);
            } else if (!strcmp(argv[i], "--01")) {
                i++;
                sfx_01 = read_number(argv[i], 256);
            } else if (!strcmp(argv[i], "--sfx")) {
                i++;
                sfx_addr = read_number(argv[i], 65536);
                sfx = TRUE;
                ctx.inplace = FALSE;
     //       } else if (!strcmp(argv[i], "-x")) {
     //           i++;
     //           compressor_path = argv[i];
            } else if (!strcmp(argv[i], "-o")) {
                i++;
                output_name = argv[i];
            } else {
                fprintf(stderr, "Error: Unknown option %s\n", argv[i]);
                exit(1);
            }
        } else if (i == argc - 1) {
            input_name = argv[i];
        } else {
            fprintf(stderr, "Error: Unknown option %s\n", argv[i]);
            exit(1);
        }
    }


    printf("dali v0.2 - a zx0-reencoder for bitfire by Tobias Bindhammer\n");
    printf("underlying zx0-packer salvador by Emmanuel Marty\n");

    if (argc == 1) {
        fprintf(stderr, "Usage: %s [options] input\n"
     //                   "  -x [path]                  Path to zx0/salvador executeable\n"
                        "  -o [filename]              Set output filename\n"
                        "  --sfx [num]                Create a c64 compatible sfx-executable\n"
                        "  --01 [num]                 Set $01 to [num] after sfx\n"
                        "  --no-inplace               Disable inplace-decompression\n"
                        "  --binfile                  Input file is a raw binary without load-address\n"
                        "  --from [$num]              Compress file from [num] on\n"
                        "  --to [$num]                Compress file until position [num]\n"
                        "  --use-prefix               Use preceeding data of file as dictionary\n"
                        "  --relocate-packed [num]    Relocate packed data to desired address [num] (resulting file can't de decompressed inplace!)\n"
                        "  --relocate-origin [num]    Set load-address of source file to [num] prior to compression. If used on bin-files, load-address and depack-target is prepended on output.\n"
                        ,argv[0]);
        exit(1);
    }

    //if (compressor_path == NULL) {
    //    fprintf(stderr, "Error: No compressor path given\n");
    //    exit(1);
   // }

    if (input_name == NULL) {
        fprintf(stderr, "Error: No input-filename given\n");
        exit(1);
    }

    /* determine output filename */
    if (output_name == NULL) {
        output_name = (char *)malloc(strlen(input_name) + 4);
        strcpy(output_name, input_name);
        strcat(output_name, ".lz");
        printf("output name: %s\n", output_name);
    }

    ctx.packed_data = (unsigned char *)malloc(BUFFER_SIZE);
    ctx.unpacked_data = (unsigned char *)malloc(BUFFER_SIZE + 2);
    ctx.reencoded_data = (unsigned char *)malloc(BUFFER_SIZE);

    if (!ctx.packed_data || !ctx.unpacked_data || !ctx.reencoded_data) {
        fprintf(stderr, "Error: Insufficient memory\n");
        exit(1);
    }

    /* load unpacked file */
    ctx.unpacked_fp = fopen(input_name, "rb");
    if (!ctx.unpacked_fp) {
        fprintf(stderr, "Error: Cannot access input file\n");
        perror("fopen");
        exit(1);
    }
    ctx.unpacked_size = fread(ctx.unpacked_data, sizeof(char), BUFFER_SIZE + 2, ctx.unpacked_fp);
    fclose(ctx.unpacked_fp);

    if (cbm_relocate_origin_addr >= 0) {
        cbm_orig_addr = cbm_relocate_origin_addr;
    } else {
        cbm_orig_addr = ctx.unpacked_data[0] + (ctx.unpacked_data[1] << 8);
    }

    if (cbm) {
      ctx.unpacked_data += 2;
      ctx.unpacked_size -= 2;
    }

    if (cbm_range_from < 0) cbm_range_from = cbm_orig_addr;
    if (cbm_range_to < 0) cbm_range_to = cbm_orig_addr + ctx.unpacked_size;

    if ((cbm_range_to - cbm_orig_addr) > ctx.unpacked_size) {
        cbm_range_to = ctx.unpacked_size + cbm_orig_addr;
        fprintf(stderr, "Warning: File ends at $%04x, adopting --to value\n", cbm_range_to);
    }
    ctx.unpacked_size = (cbm_range_to - cbm_orig_addr);

    /* if range is below start_address, adopt range */
    if (cbm_range_from < cbm_orig_addr) {
        cbm_range_from = cbm_orig_addr;
        fprintf(stderr, "Warning: File starts at $%04x, adopting --from value\n", cbm_range_from);
    }
    /* load file from start_pos on only, so skip bytes on input */
    ctx.unpacked_data += (cbm_range_from - cbm_orig_addr);
    /* also adopt ctx.unpacked_size */
    ctx.unpacked_size -= (cbm_range_from - cbm_orig_addr);

    cbm_orig_addr = cbm_range_from;

    if (cbm_range_from > cbm_range_to) {
        fprintf(stderr, "Error: --from beyond fileend\n");
        exit(1);
    }

    if (ctx.unpacked_size < 2) {
        fprintf(stderr, "Error: Input too small\n");
        exit(1);
    }

    printf("Compressing from $%04x to $%04x = $%04lx bytes\n", cbm_range_from, cbm_range_to, ctx.unpacked_size);

    if (cbm_relocate_packed_addr >= 0) {
        ctx.inplace = FALSE;
    }

    if (sfx) {
        ctx.inplace = FALSE;
        printf("Creating sfx with start-address $%04x\n", sfx_addr);
    }

    /* write clamped raw data */
    ctx.clamped_fp = fopen(output_name, "wb");
    if (!ctx.clamped_fp) {
        fprintf(stderr, "Error: Cannot create clamped file (%s)\n", output_name);
        perror("fopen");
        exit(1);
    }
    if (ctx.unpacked_size != 0) {
        if (fwrite(ctx.unpacked_data, sizeof(char), ctx.unpacked_size, ctx.clamped_fp) != ctx.unpacked_size) {
            fprintf(stderr, "Error: Cannot write clamped file\n");
            perror("fwrite");
            exit(1);
        }
    }
    fclose(ctx.clamped_fp);

    /* compress data with salvador */
    salvador_argv[0] = "salvador";
    salvador_argv[1] = output_name;
    salvador_argv[2] = output_name;
    salvador_main(3, salvador_argv);
    //shell_call = (char *)malloc(strlen(output_name) + strlen(output_name) + strlen(compressor_path) + 3);
    //sprintf(shell_call, "%s %s %s", compressor_path, output_name, output_name);
    //if (system(shell_call) == -1) {
    //    fprintf(stderr, "Error: Cannot execute %s\n", shell_call);
    //    exit(1);
    //}

    /* read packed data */
    ctx.packed_fp = fopen(output_name, "rb");
    if (!ctx.packed_fp) {
        fprintf(stderr, "Error: Cannot access input file\n");
        perror("fopen");
        exit(1);
    }
    ctx.packed_size = fread(ctx.packed_data, sizeof(char), BUFFER_SIZE, ctx.packed_fp);
    fclose(ctx.packed_fp);

    /* determine size without eof-marker -> remove 18 bits, either 2 byte or three byte depending on position of last bitpair */
    if (ctx.packed_data[ctx.packed_size - 1] & 0x80) ctx.packed_size -= 3;
    else ctx.packed_size -= 2;

    /* write reencoded output file */
    ctx.reencoded_fp = fopen(output_name, "wb");
    if (!ctx.reencoded_fp) {
        fprintf(stderr, "Error: Cannot create reencoded file (%s)\n", output_name);
        perror("fopen");
        exit(1);
    }
    reencode(&ctx);

    /* as sfx */
    if (sfx) {
        /* copy over to change values in code */
        sfx_code = (char *)malloc(sizeof(decruncher));
        memcpy (sfx_code, decruncher, sizeof(decruncher));

        /* setup jmp target after decompression */
        sfx_code[ZX0_SFX_ADDR + 0] = sfx_addr & 0xff;
        sfx_code[ZX0_SFX_ADDR + 1] = (sfx_addr >> 8) & 0xff;

        /* setup decompression destination */
        sfx_code[ZX0_DST + 0] = cbm_orig_addr & 0xff;
        sfx_code[ZX0_DST + 1] = (cbm_orig_addr >> 8) & 0xff;

        /* setup compressed data src */
        sfx_code[ZX0_SRC + 0] = (0x10000 - ctx.reencoded_index) & 0xff;
        sfx_code[ZX0_SRC + 1] = ((0x10000 - ctx.reencoded_index) >> 8) & 0xff;

        /* setup compressed data end */
        sfx_code[ZX0_DATA_END + 0] = (0x0801 + sizeof(decruncher) - 2 + ctx.reencoded_index - 0x100) & 0xff;
        sfx_code[ZX0_DATA_END + 1] = ((0x0801 + sizeof(decruncher) - 2 + ctx.reencoded_index - 0x100) >> 8) & 0xff;

        sfx_code[ZX0_DATA_SIZE_HI] = 0xff - (((ctx.reencoded_index + 0x100) >> 8) & 0xff);

        sfx_code[ZX0_01] = sfx_01;

        printf("original: $%04x-$%04lx ($%04lx) 100%%\n", cbm_orig_addr, cbm_orig_addr + ctx.unpacked_size, ctx.unpacked_size);
        printf("packed:   $%04x-$%04lx ($%04lx) %3.2f%%\n", 0x0801, 0x0801 + (int)sizeof(decruncher) + ctx.packed_index, (int)sizeof(decruncher) + ctx.packed_index, ((float)(ctx.packed_index + (int)sizeof(decruncher)) / (float)(ctx.unpacked_size) * 100.0));

        if (fwrite(sfx_code, sizeof(char), sizeof(decruncher), ctx.reencoded_fp) != sizeof(decruncher)) {
            fprintf(stderr, "Error: Cannot write output file %s\n", output_name);
            exit(1);
        }
        if (fwrite(ctx.reencoded_data, sizeof(char), ctx.reencoded_index, ctx.reencoded_fp) != ctx.reencoded_index) {
            fprintf(stderr, "Error: Cannot write output file %s\n", output_name);
            exit(1);
        }
    /* or standard compressed */
    } else {
        if (cbm_relocate_origin_addr >= 0) {
            cbm_orig_addr = cbm_relocate_origin_addr;
            cbm = TRUE;
        }

        if (ctx.inplace) {
            cbm_packed_addr = cbm_range_to - ctx.packed_index - 2;
        } else {
            if (cbm_relocate_packed_addr >= 0) {
                cbm_packed_addr = cbm_relocate_packed_addr;
            } else {
                cbm_packed_addr = cbm_orig_addr;
            }
        }

        printf("original: $%04x-$%04lx ($%04lx) 100%%\n", cbm_orig_addr, cbm_orig_addr + ctx.unpacked_size, ctx.unpacked_size);
        printf("packed:   $%04x-$%04lx ($%04lx) %3.2f%%\n", cbm_packed_addr, cbm_packed_addr + ctx.packed_index + 2, ctx.packed_index + 2, ((float)(ctx.packed_index) / (float)(ctx.unpacked_size) * 100.0));

        if (cbm) {
            if ((cbm_packed_addr >= 0xd000 && cbm_packed_addr < 0xe000) || (cbm_packed_addr < 0xd000 && cbm_packed_addr + ctx.packed_index + 2 > 0xd000)) {
                fprintf(stderr, "Error: Packed file lies in I/O-range from $d000-$dfff\n");
                exit(1);
            }
        }
        save_reencoded(&ctx, cbm_orig_addr, cbm_packed_addr);
    }
    fclose(ctx.reencoded_fp);

    if (ctx.packed_index + 2 > ctx.unpacked_size) {
        fprintf(stderr, "Error: Packed file larger than original\n");
        exit(1);
    }

    return 0;
}
