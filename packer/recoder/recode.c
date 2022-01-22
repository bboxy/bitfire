#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "sfx.h"

#define BUFFER_SIZE 65536  /* must be > MAX_OFFSET */
#define INITIAL_OFFSET 1

#define FALSE 0
#define TRUE 1

typedef struct ctx {
    FILE *pfp;
    //FILE *ofp;
    FILE *rfp;
    FILE *ufp;
    FILE *cfp;
    unsigned char *packed_data;
    unsigned char *reencoded_data;
    unsigned char *unpacked_data;
    size_t packed_index;
    size_t packed_size;
    size_t unpacked_index;
    size_t unpacked_size;
    size_t reencoded_index;
    size_t reencoded_size;
    int reencoded_bit_mask;
    int reencoded_bit_value;
    int reencoded_bit_index;
    int bit_mask;
    int bit_value;
    int last_byte;
    int inplace;
} ctx;

static int read_number(char* arg) {
    int number;
    if(arg[0] == '$') number = strtoul(arg + 1, NULL, 16);
    else if(arg[0] == '0' && arg[1] == 'x') number = strtoul(arg + 2, NULL, 16);
    else number = strtoul(arg, NULL, 10);
    if (number < 0 || number > 65535) {
        fprintf(stderr, "Error: Number '%s' out of range (0 - 65536)\n", arg);
        exit(1);
    }
    return number;
}

static void file_write_byte(int byte, FILE *ofp) {
    if (fputc(byte, ofp) != byte) {
        fprintf(stderr, "Error: Cannot write output file\n");
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
    if (!ctx->reencoded_bit_mask) {
        ctx->reencoded_bit_mask = 128;
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
    int count = 0;

    for (i = 2; i <= value; i <<= 1)
        ;
    i >>= 1;

    if (bits >= 8) {
        /* change bit-order, send LSB first */
        /* remove preceeding 1 first */
        value = value & ((0xffff ^ i));
        /* move LSB bits to the beginning */
        value = (value >> 8) | ((value & 0xff) << (bits - 8));
    }

    while ((i >>= 1) > 0) {
        //if (count >= 8) invert = 0;
        count++;
        if (!skip) write_reencoded_bit(ctx, 0);
        skip = 0;
        if (!skip) write_reencoded_bit(ctx, ((value & i) > 0));
    }
    if (!skip) write_reencoded_bit(ctx, 1);
}

//XXX TODO aggregate write_*_byte by handing over correct buffer?
void write_reencoded_offset(ctx* ctx, int offset, int length) {
    write_reencoded_byte(ctx, (((offset - 1) % 128) << 1) | (length == 2));
}

int read_byte(ctx* ctx) {
    ctx->last_byte = ctx->packed_data[ctx->packed_index++];
    return ctx->last_byte;
}

int read_bit(ctx* ctx) {
    ctx->bit_mask >>= 1;
    if (ctx->bit_mask == 0) {
        ctx->bit_mask = 128;
        ctx->bit_value = read_byte(ctx);
    }
    return ctx->bit_value & ctx->bit_mask ? 1 : 0;
}

int read_interlaced_elias_gamma(ctx* ctx, int inverted, int skip) {
    int value = 1;
    while (skip || !read_bit(ctx)) {
        skip = 0;
        value = (value << 1) | (read_bit(ctx) ^ inverted);
    }
    return value;
}

void save_reencoded(ctx* ctx, int cbm_orig_addr, int cbm_packed_addr) {
    if (ctx->packed_index != 0) {
        /* little endian */
        file_write_byte(cbm_packed_addr & 255, ctx->rfp);
        file_write_byte((cbm_packed_addr >> 8) & 255, ctx->rfp);
        /* big endian, as read backwards by depacker */
        file_write_byte((cbm_orig_addr >> 8) & 255, ctx->rfp);
        file_write_byte(cbm_orig_addr & 255, ctx->rfp);

        if (fwrite(ctx->reencoded_data, sizeof(char), ctx->packed_index, ctx->rfp) != ctx->packed_index) {
            fprintf(stderr, "Error: Cannot write output file\n");
            exit(1);
        }
    }
}

void read_packed(ctx* ctx) {
    ctx->packed_size = fread(ctx->packed_data, sizeof(char), BUFFER_SIZE, ctx->pfp);
}

void copy_inplace_literal(ctx* ctx) {
    int i;
    for (i = ctx->unpacked_index; i < ctx->unpacked_size; i++) {
        ctx->reencoded_data[ctx->packed_index] = ctx->unpacked_data[i];
        ctx->packed_index++;
    }
}

void find_inplace(ctx* ctx) {
    int last_offset = INITIAL_OFFSET;
    int length;
    int i;
    int overwrite;

    int bit, byte;

    int safe_input_index = 0;
    int safe_output_index = 0;

    ctx->packed_index = 0;
    ctx->unpacked_index = 0;

    ctx->bit_mask = 0;

COPY_LITERALS:
    length = read_interlaced_elias_gamma(ctx, FALSE, 0);
    for (i = 0; i < length; i++) {
        byte = read_byte(ctx);
    }
    ctx->unpacked_index += length;

    overwrite = (ctx->unpacked_index) - (ctx->unpacked_size - ctx->reencoded_index + ctx->packed_index);
    /* literal would overwrite packed src */
    if (overwrite >= 0) {
        /* go back to previous index */
        ctx->unpacked_index = safe_input_index;
        ctx->packed_index = safe_output_index;
        return;
    }
    /* do remember last safe position */
    safe_input_index = ctx->unpacked_index;
    safe_output_index = ctx->packed_index;

    bit = read_bit(ctx);
    if (bit) {
        goto COPY_FROM_NEW_OFFSET;
    }

//COPY_FROM_LAST_OFFSET:
    length = read_interlaced_elias_gamma(ctx, FALSE, 0);
    ctx->unpacked_index += length;

    overwrite = (ctx->unpacked_index) - (ctx->unpacked_size - ctx->reencoded_index + ctx->packed_index);
    /* rep would overwrite packed src */
    if (overwrite >= 0) {
        safe_input_index = ctx->unpacked_index;
        safe_output_index = ctx->packed_index;
        ctx->unpacked_index = safe_input_index;
        ctx->packed_index = safe_output_index;
        return;
    }
    safe_input_index = ctx->unpacked_index;
    safe_output_index = ctx->packed_index;

    bit = read_bit(ctx);
    if (!bit) {
        goto COPY_LITERALS;
    }

COPY_FROM_NEW_OFFSET:
    last_offset = read_interlaced_elias_gamma(ctx, TRUE, 0);
    if (last_offset == 256) {
        if (ctx->packed_index != ctx->packed_size) {
            fprintf(stderr, "Error: Input file too long\n");
            exit(1);
        }
        return;
    }
    byte = read_byte(ctx);
    last_offset = last_offset * 128 - (byte >> 1);
    if (byte & 1) length = 2;
    else length = read_interlaced_elias_gamma(ctx, FALSE, 1) + 1;

    ctx->unpacked_index += length;

    overwrite = (ctx->unpacked_index) - (ctx->unpacked_size - ctx->reencoded_index + ctx->packed_index);
    /* rep would overwrite packed src */
    if (overwrite >= 0) {
        safe_input_index = ctx->unpacked_index;
        safe_output_index = ctx->packed_index;
        ctx->unpacked_index = safe_input_index;
        ctx->packed_index = safe_output_index;
        return;
    }
    safe_input_index = ctx->unpacked_index;
    safe_output_index = ctx->packed_index;

    bit = read_bit(ctx);
    if (bit) {
        goto COPY_FROM_NEW_OFFSET;
    } else {
        goto COPY_LITERALS;
    }
}

void reencode(ctx* ctx) {
    int last_offset = INITIAL_OFFSET;
    int length;
    int i;

    int bit, byte;
    int first = 1;

    ctx->packed_index = 0;
    ctx->bit_mask = 0;

    ctx->reencoded_index = 0;
    ctx->reencoded_bit_mask = 0;
    ctx->reencoded_bit_value = 0;
    ctx->reencoded_bit_index = 0;

COPY_LITERALS:
    length = read_interlaced_elias_gamma(ctx, FALSE, 0);
    if (!first) write_reencoded_bit(ctx, 0);
    first = 0;
    write_reencoded_interlaced_elias_gamma(ctx, length, 0);
    for (i = 0; i < length; i++) {
        byte = read_byte(ctx);
        write_reencoded_byte(ctx, byte);
    }

    bit = read_bit(ctx);

    if (bit) {
        goto COPY_FROM_NEW_OFFSET;
    }

//COPY_FROM_LAST_OFFSET:
    length = read_interlaced_elias_gamma(ctx, FALSE, 0);
    write_reencoded_bit(ctx, 0);
    write_reencoded_interlaced_elias_gamma(ctx, length, 0);

    bit = read_bit(ctx);

    if (!bit) {
        goto COPY_LITERALS;
    }

COPY_FROM_NEW_OFFSET:
    last_offset = read_interlaced_elias_gamma(ctx, TRUE, 0);

    if (last_offset == 256) {
        /* omit end-marker when packing inplace */
        if (!ctx->inplace) {
            write_reencoded_bit(ctx, 1);
            write_reencoded_interlaced_elias_gamma(ctx, last_offset, 0);
        }
        if (ctx->packed_index != ctx->packed_size) {
            fprintf(stderr, "Error: Input file too long\n");
            exit(1);
        }
        return;
    }

    write_reencoded_bit(ctx, 1);
    write_reencoded_interlaced_elias_gamma(ctx, last_offset, 0);

    byte = read_byte(ctx);
    last_offset = last_offset * 128 - (byte >> 1);
    if (byte & 1) length = 2;
    else length = read_interlaced_elias_gamma(ctx, FALSE, 1) + 1;

    write_reencoded_offset(ctx, last_offset, length);
    write_reencoded_interlaced_elias_gamma(ctx, length - 1, 1);

    bit = read_bit(ctx);

    if (bit) {
        goto COPY_FROM_NEW_OFFSET;
    } else {
        goto COPY_LITERALS;
    }
}

int main(int argc, char *argv[]) {
    int cbm_orig_addr = 0;
    int cbm_packed_addr = 0;
    int cbm_range_from = -1;
    int cbm_range_to = -1;
    int cbm_relocate_packed_addr = -1;
    int cbm_relocate_origin_addr = -1;
    int file_start_pos = 0;

    int sfx = FALSE;
    int sfx_addr = -1;
    char *sfx_code = NULL;

    char *output_name = NULL;
    char *input_name = NULL;
    char *compressor_path = NULL;
    char *shell_call = NULL;

    int cbm = TRUE;

    int i;

    FILE *cfp;

    ctx ctx;

    ctx.inplace = TRUE;

    for (i = 1; i < argc; i++) {
        if (!strncmp(argv[i], "-", 1) || !strncmp(argv[i], "--", 2)) {
            if (!strcmp(argv[i], "--binfile")) {
                cbm = FALSE;
            } else if (!strcmp(argv[i], "--no-inplace")) {
                ctx.inplace = FALSE;
            } else if (!strcmp(argv[i], "--relocate-packed")) {
                i++;
                cbm_relocate_packed_addr = read_number(argv[i]);
            } else if (!strcmp(argv[i], "--relocate-origin")) {
                i++;
                cbm_relocate_origin_addr = read_number(argv[i]);
            } else if (!strcmp(argv[i], "--from")) {
                i++;
                cbm_range_from = read_number(argv[i]);
            } else if (!strcmp(argv[i], "--to")) {
                i++;
                cbm_range_to = read_number(argv[i]);
            } else if (!strcmp(argv[i], "--sfx")) {
                i++;
                sfx_addr = read_number(argv[i]);
                sfx = TRUE;
                ctx.inplace = FALSE;
            } else if (!strcmp(argv[i], "-x")) {
                i++;
                compressor_path = argv[i];
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

    printf("zx0-recoder for bitfire v0.1 by Tobias Bindhammer\n");

    if (argc == 1) {
        fprintf(stderr, "Usage: %s [options] input\n"
                        "  -x [path]                  Path to zx0/salvador executeable\n"
                        "  -o [filename]              Set output filename\n"
                        "  --sfx [num]                Create a c64 compatible sfx-executable\n"
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
    //ctx.output_data = (unsigned char *)malloc(BUFFER_SIZE);
    ctx.reencoded_data = (unsigned char *)malloc(BUFFER_SIZE);

    if (!ctx.packed_data || !ctx.unpacked_data || !ctx.reencoded_data) {
        fprintf(stderr, "Error: Insufficient memory\n");
        exit(1);
    }

    ctx.ufp = fopen(input_name, "rb");
    if (!ctx.ufp) {
        fprintf(stderr, "Error: Cannot access input file\n");
        exit(1);
    }

    ctx.unpacked_size = fread(ctx.unpacked_data, sizeof(char), BUFFER_SIZE + 2, ctx.ufp);
    fclose(ctx.ufp);

    /* load unpacked file */
    if (cbm_relocate_origin_addr >= 0) {
        cbm_orig_addr = cbm_relocate_origin_addr;
    } else {
        cbm_orig_addr = ctx.unpacked_data[0] + (ctx.unpacked_data[1] << 8);
    }

    file_start_pos = 2;
    ctx.unpacked_size -= 2;

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
    file_start_pos += (cbm_range_from - cbm_orig_addr);
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

    ctx.unpacked_data += file_start_pos;

    cfp = fopen(output_name, "wb");
    if (ctx.unpacked_size != 0) {
        if (fwrite(ctx.unpacked_data, sizeof(char), ctx.unpacked_size, cfp) != ctx.unpacked_size) {
            fprintf(stderr, "Error: Cannot write clamped file\n");
            exit(1);
        }
    }
    fclose(cfp);

    shell_call = (char *)malloc(strlen(output_name) + strlen(output_name) + strlen(compressor_path) + 3);
    sprintf(shell_call, "%s %s %s", compressor_path, output_name, output_name);
    if (system(shell_call) == -1) {
        fprintf(stderr, "Error: Cannot execute %s\n", shell_call);
        exit(1);
    }

    ctx.pfp = fopen(output_name, "rb");
    if (!ctx.pfp) {
        fprintf(stderr, "Error: Cannot access input file\n");
        exit(1);
    }
    read_packed(&ctx);
    fclose(ctx.pfp);

    ctx.rfp = fopen(output_name, "wb");
    if (!ctx.rfp) {
        fprintf(stderr, "Error: Cannot create output file\n");
        exit(1);
    }

    reencode(&ctx);
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

        sfx_code[ZX0_DATA_SIZE_HI] = ((ctx.reencoded_index + 0x100) >> 8) & 0xff;

        printf("original: $%04x-$%04lx ($%04lx) 100%%\n", cbm_orig_addr, cbm_orig_addr + ctx.unpacked_size, ctx.unpacked_size);
        printf("packed:   $%04x-$%04lx ($%04lx) %3.2f%%\n", 0x0801, 0x0801 + (int)sizeof(decruncher) + ctx.packed_index, (int)sizeof(decruncher) + ctx.packed_index, ((float)(ctx.packed_index + (int)sizeof(decruncher)) / (float)(ctx.unpacked_size) * 100.0));

        if (fwrite(sfx_code, sizeof(char), sizeof(decruncher), ctx.rfp) != sizeof(decruncher)) {
            fprintf(stderr, "Error: Cannot write output file %s\n", output_name);
            exit(1);
        }
        if (fwrite(ctx.reencoded_data, sizeof(char), ctx.reencoded_index, ctx.rfp) != ctx.reencoded_index) {
            fprintf(stderr, "Error: Cannot write output file %s\n", output_name);
            exit(1);
        }
    } else {
        if (ctx.inplace) {
            /* now as we know the packed-size from first pass, do a second pass to find out where src- and dst-streams meet */
            find_inplace(&ctx);
            copy_inplace_literal(&ctx);
        }

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
    fclose(ctx.rfp);

    return 0;
}
