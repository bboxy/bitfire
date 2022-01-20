/*
 * ZX0 decompressor - by Einar Saukas
 * https://github.com/einar-saukas/ZX0
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BUFFER_SIZE 65536  /* must be > MAX_OFFSET */
#define INITIAL_OFFSET 1

#define FALSE 0
#define TRUE 1

typedef struct ctx {
    FILE *ifp;
    FILE *ofp;
    FILE *rfp;
    unsigned char *input_data;
    unsigned char *output_data;
    unsigned char *reencoded_data;
    size_t input_index;
    size_t output_index;
    size_t input_size;
    size_t output_size;
    size_t reencoded_size;
    size_t reencoded_index;
    int reencoded_bit_mask;
    int reencoded_bit_value;
    int reencoded_bit_index;
    int bit_mask;
    int bit_value;
    int backtrack;
    int last_byte;
} ctx;

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
    ctx->last_byte = ctx->input_data[ctx->input_index++];
    return ctx->last_byte;
}

int read_bit(ctx* ctx) {
    if (ctx->backtrack) {
        ctx->backtrack = FALSE;
        return ctx->last_byte & 1;
    }
    ctx->bit_mask >>= 1;
    if (ctx->bit_mask == 0) {
        ctx->bit_mask = 128;
        ctx->bit_value = read_byte(ctx);
    }
    return ctx->bit_value & ctx->bit_mask ? 1 : 0;
}

int read_interlaced_elias_gamma(ctx* ctx, int inverted) {
    int value = 1;
    while (!read_bit(ctx)) {
        value = (value << 1) | (read_bit(ctx) ^ inverted);
    }
    return value;
}

void save_reencoded(ctx* ctx) {
    if (ctx->reencoded_index != 0) {
        if (fwrite(ctx->reencoded_data, sizeof(char), ctx->reencoded_index, ctx->rfp) != ctx->reencoded_index) {
            fprintf(stderr, "Error: Cannot write output file\n");
            exit(1);
        }
        ctx->reencoded_size = ctx->reencoded_index;
        ctx->reencoded_index = 0;
    }
}

void save_output(ctx* ctx) {
    if (ctx->output_index != 0) {
        if (fwrite(ctx->output_data, sizeof(char), ctx->output_index, ctx->ofp) != ctx->output_index) {
            fprintf(stderr, "Error: Cannot write output file\n");
            exit(1);
        }
        ctx->output_size = ctx->output_index;
        ctx->output_index = 0;
    }
}

void write_byte(ctx* ctx, int value) {
    ctx->output_data[ctx->output_index++] = value;
    //XXX TODO check for index >= BUFFER_SIZE and bail out
}

void write_bytes(ctx* ctx, int offset, int length) {
    int i;

    if (offset > ctx->output_index) {
        fprintf(stderr, "Error: Invalid data in input file\n");
        exit(1);
    }
    while (length-- > 0) {
        i = ctx->output_index - offset;
        write_byte(ctx, ctx->output_data[i >= 0 ? i : BUFFER_SIZE + i]);
    }
}

void read_input(ctx* ctx) {
    ctx->input_size = fread(ctx->input_data, sizeof(char), BUFFER_SIZE, ctx->ifp);
}

void decompress(ctx* ctx) {
    int last_offset = INITIAL_OFFSET;
    int length;
    int i;

    int bit, byte;

    ctx->input_index = 0;
    ctx->output_index = 0;

    ctx->bit_mask = 0;
    ctx->backtrack = FALSE;

    ctx->reencoded_index = 0;
    ctx->reencoded_size = 0;
    ctx->reencoded_bit_mask = 0;
    ctx->reencoded_bit_value = 0;
    ctx->reencoded_bit_index = 0;

COPY_LITERALS:
    length = read_interlaced_elias_gamma(ctx, FALSE);

    write_reencoded_interlaced_elias_gamma(ctx, length, 0);
    for (i = 0; i < length; i++) {
        byte = read_byte(ctx);
        write_byte(ctx, byte);
        write_reencoded_byte(ctx, byte);
    }
    bit = read_bit(ctx);
    write_reencoded_bit(ctx, bit);
    if (bit) {
        goto COPY_FROM_NEW_OFFSET;
    }

/*COPY_FROM_LAST_OFFSET:*/
    length = read_interlaced_elias_gamma(ctx, FALSE);

    write_reencoded_interlaced_elias_gamma(ctx, length, 0);

    write_bytes(ctx, last_offset, length);

    bit = read_bit(ctx);
    write_reencoded_bit(ctx, bit);
    if (!bit) {
        goto COPY_LITERALS;
    }

COPY_FROM_NEW_OFFSET:
    last_offset = read_interlaced_elias_gamma(ctx, TRUE);

    write_reencoded_interlaced_elias_gamma(ctx, last_offset, 0);

    if (last_offset == 256) {
        if (ctx->input_index != ctx->input_size) {
            fprintf(stderr, "Error: Input file too long\n");
            exit(1);
        }
        return;
    }
    byte = read_byte(ctx);
    ctx->backtrack = TRUE;
    last_offset = last_offset * 128 - (byte >> 1);

    length = read_interlaced_elias_gamma(ctx, FALSE) + 1;

    write_reencoded_offset(ctx, last_offset, length);	//XXX TODO write 8 bits only
    write_reencoded_interlaced_elias_gamma(ctx, length - 1, 1);
    write_bytes(ctx, last_offset, length);

    bit = read_bit(ctx);

    write_reencoded_bit(ctx, bit);

    if (bit) {
        goto COPY_FROM_NEW_OFFSET;
    } else {
        goto COPY_LITERALS;
    }
}

int main(int argc, char *argv[]) {
    ctx ctx;

    ctx.ifp = fopen("testfile.zx0", "rb");
    if (!ctx.ifp) {
        fprintf(stderr, "Error: Cannot access input file\n");
        exit(1);
    }

    ctx.ofp = fopen("testfile.prg", "wb");
    if (!ctx.ofp) {
        fprintf(stderr, "Error: Cannot create output file\n");
        exit(1);
    }

    ctx.rfp = fopen("testfile.bitfire", "wb");
    if (!ctx.rfp) {
        fprintf(stderr, "Error: Cannot create output file\n");
        exit(1);
    }

    ctx.input_data = (unsigned char *)malloc(BUFFER_SIZE);
    ctx.output_data = (unsigned char *)malloc(BUFFER_SIZE);
    ctx.reencoded_data = (unsigned char *)malloc(BUFFER_SIZE);

    if (!ctx.input_data || !ctx.output_data || !ctx.reencoded_data) {
        fprintf(stderr, "Error: Insufficient memory\n");
        exit(1);
    }

    read_input(&ctx);
    decompress(&ctx);
    printf("reencoded_size: %d\n", ctx.reencoded_index);
    save_output(&ctx);
    save_reencoded(&ctx);
    fclose(ctx.ifp);
    fclose(ctx.ofp);
    fclose(ctx.rfp);

    return 0;
}
