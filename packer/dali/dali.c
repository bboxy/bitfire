#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "sfx.h"

#define BUFFER_SIZE 65536  /* must be > MAX_OFFSET */
#define INITIAL_OFFSET 1

#define FALSE 0
#define TRUE 1
#define DALI_BITS_LEFT 1
#define DALI_ELIAS_LE 1
#define DALI_VARS_SIZE 26

#define POS_DALI_SFX_SRC 	0
#define POS_DALI_SRC 		2
#define POS_DALI_DST 		4
#define POS_DALI_SFX_ADDR 	6
#define POS_DALI_DATA_END 	8
#define POS_DALI_MAX_MEM 	10
#define POS_DALI_DATA_SIZE_HI 	12
#define POS_DALI_01 		14
#define POS_DALI_CLI 		16
#define POS_DALI_EFFECT		18
#define POS_DALI_STUB_SIZE	20
#define POS_DALI_INIT_SIZE	22
#define POS_DALI_SFX_DATA	24

#define SFX_C64			1
#define SFX_PLUS4		2
#define SFX_VC20		3

//include salvador and rename main
#define main salvador_main
#include "salvador/src/salvador.c"
#undef main

typedef struct ctx {
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
    int force_inplace;

    char *output_name;
    char *input_name;
    char *prefix_name;
    char *clamped_name;

    int cbm;
    int cbm_orig_addr;
    int cbm_packed_addr;
    int cbm_range_from;
    int cbm_range_to;
    int cbm_relocate_packed_addr;
    int cbm_relocate_end_addr;
    int cbm_relocate_origin_addr;
    int cbm_relocate_sfx_addr;
    int cbm_prefix_from;

    int sfx;
    int sfx_addr;
    int sfx_01;
    int sfx_cli;
    int sfx_small;
    int sfx_effect;
    int lz_bits;

    int exit_on_warn;
} ctx;

static int read_number(char* arg, char* argname, int limit) {
    int number;
    if (arg != NULL && arg[0] == '$') number = strtoul(arg + 1, NULL, 16);
    else if (arg != NULL && arg[0] == '0' && arg[1] == 'x') number = strtoul(arg + 2, NULL, 16);
    else if (arg != NULL && arg[0] >= '0' && arg[0] <= '9') number = strtoul(arg, NULL, 10);
    else {
        fprintf(stderr, "Error: no valid number given for argument %s (given value is: '%s')\n", argname, arg);
        exit(1);
    }
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
        if (DALI_BITS_LEFT == 1) {
            ctx->reencoded_bit_mask = 0x80;
        } else {
            ctx->reencoded_bit_mask = 0x1;
        }
        /* remember position of bit-buffer */
        ctx->reencoded_bit_index = ctx->reencoded_index;
        ctx->lz_bits++;
        write_reencoded_byte(ctx, 0);
    }
    if (value)
        ctx->reencoded_data[ctx->reencoded_bit_index] |= ctx->reencoded_bit_mask;
    if (DALI_BITS_LEFT == 1) {
        ctx->reencoded_bit_mask >>= 1;
    } else {
        ctx->reencoded_bit_mask <<= 1;
    }
}

void write_reencoded_interlaced_elias_gamma(ctx* ctx, int value, int skip) {
    int bits = bit_size(value);
    int i;

    for (i = 2; i <= value; i <<= 1);
    i >>= 1;

    if (DALI_ELIAS_LE) {
        if (bits >= 8) {
            /* change bit-order, send LSB first */
            /* remove preceeding 1 first */
            value = value & ((0xffff ^ i));
            /* move LSB bits to the beginning */
            value = (value >> 8) | ((value & 0xff) << (bits - 8));
        }
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

void save_reencoded_stream(ctx* ctx) {
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

void reencode_packed_stream(ctx* ctx) {
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
    ctx->lz_bits = 0;

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

unsigned int get_var(unsigned char* code, unsigned int pos) {
    return code[pos] + (code[pos + 1] << 8);
}

void write_reencoded_stream(ctx* ctx) {
    FILE *fp = NULL;
    unsigned int dali_data_end_addr;
    unsigned int dali_load_addr;
    unsigned int dali_sfx_src;
    unsigned int dali_src;
    unsigned int dali_dst;
    unsigned int dali_sfx_addr;
    unsigned int dali_data_end;
    unsigned int dali_max_mem;
    unsigned int dali_data_size_hi;
    unsigned int dali_01;
    unsigned int dali_cli;
    unsigned int dali_init_size;
    unsigned int dali_stub_size;
    unsigned int dali_sfx_data;
    unsigned int dali_sfx_src_addr;

    unsigned char* sfx_code_start;
    unsigned int sfx_code_size;

    const char* sfx_code_version;
    unsigned char* sfx_code;
    unsigned int sfx_size;

    //unsigned int dali_load_addr;
    //int dali_effect_code;

    int before_reloc = -1;

    //unsigned int var_dali_effect_code;

    /* write reencoded output file */
    fp = fopen(ctx->output_name, "wb");
    if (!fp) {
        fprintf(stderr, "Error: Cannot create reencoded file (%s)\n", ctx->output_name);
        exit(1);
    }

    /* as sfx */
    if (ctx->sfx) {
        printf("Creating sfx with start-address $%04x\n", ctx->sfx_addr);

        sfx_code_version = decruncher;
        sfx_size = sizeof(decruncher);

        if (ctx->sfx == SFX_C64) {
            if (ctx->sfx_small) {
                if (ctx->sfx_effect) {
                    sfx_code_version = decruncher_small_effect;
                    sfx_size = sizeof(decruncher_small_effect);
                } else {
                    sfx_code_version = decruncher_small;
                    sfx_size = sizeof(decruncher_small);
                }
            } else {
                if (ctx->sfx_effect) {
                    sfx_code_version = decruncher_effect;
                    sfx_size = sizeof(decruncher_effect);
                } else {
                    sfx_code_version = decruncher;
                    sfx_size = sizeof(decruncher);
                }
            }
        } else if (ctx->sfx == SFX_PLUS4) {
            //if (ctx->sfx_small) {
            //    sfx_code_version = decruncher_plus4_small;
            //    sfx_size = sizeof(decruncher_plus4_small);
            //} else {
                sfx_code_version = decruncher_plus4;
                sfx_size = sizeof(decruncher_plus4);
            //}
        } else if (ctx->sfx == SFX_VC20) {
            //if (ctx->sfx_small) {
            //    sfx_code_version = decruncher_plus4_small;
            //    sfx_size = sizeof(decruncher_plus4_small);
            //} else {
                sfx_code_version = decruncher_vc20;
                sfx_size = sizeof(decruncher_vc20);
            //}
        }


        /* copy over to change values in code */
        sfx_code = (unsigned char *)malloc(sfx_size);
        memcpy (sfx_code, sfx_code_version, sfx_size);

        sfx_size -= DALI_VARS_SIZE;

        /* fetch vars from binary */
        dali_sfx_src = get_var(sfx_code, sfx_size + POS_DALI_SFX_SRC);
        dali_src = get_var(sfx_code, sfx_size + POS_DALI_SRC);
        dali_dst = get_var(sfx_code, sfx_size + POS_DALI_DST);
        dali_sfx_addr = get_var(sfx_code, sfx_size + POS_DALI_SFX_ADDR);
        dali_data_end = get_var(sfx_code, sfx_size + POS_DALI_DATA_END);
        dali_max_mem = get_var(sfx_code, sfx_size + POS_DALI_MAX_MEM);
        dali_data_size_hi = get_var(sfx_code, sfx_size + POS_DALI_DATA_SIZE_HI);
        dali_01 = get_var(sfx_code, sfx_size + POS_DALI_01);
        dali_cli = get_var(sfx_code, sfx_size + POS_DALI_CLI);
        dali_stub_size = get_var(sfx_code, sfx_size + POS_DALI_STUB_SIZE);
        dali_init_size = get_var(sfx_code, sfx_size + POS_DALI_INIT_SIZE);
        dali_sfx_data = get_var(sfx_code, sfx_size + POS_DALI_SFX_DATA);
        //dali_effect_code = get_var(sfx_code, var_dali_effect_code);

        if (ctx->cbm_relocate_sfx_addr >= 0) {
            dali_load_addr = ctx->cbm_relocate_sfx_addr;
        } else {
            dali_load_addr = get_var(sfx_code, 0);
        }

        sfx_code_start = sfx_code + 2;
        sfx_code_size = sfx_size - 2;

        dali_data_end_addr = dali_load_addr + dali_sfx_data + ctx->reencoded_index - 0x100;
        dali_sfx_src_addr = dali_load_addr + dali_init_size;

        if (ctx->cbm_relocate_sfx_addr >= 0) {
            /* reduce data_end_addr and sfx_src_addr by stub size, as stub is omitted when relocated */
            dali_data_end_addr -= dali_stub_size;
            dali_sfx_src_addr -= dali_stub_size;
            sfx_code_start -= dali_stub_size;
            sfx_code_size -= dali_stub_size;
        }

        if (ctx->sfx == SFX_VC20 || ctx->sfx == SFX_PLUS4) {
            //printf("optimal load addr: %04x\n", ctx->cbm_range_to - (ctx->unpacked_index - ctx->unpacked_size));
            switch (ctx->cbm_range_to - (ctx->unpacked_index - ctx->unpacked_size)) {
            //switch (ctx->cbm_orig_addr + (int)ctx->unpacked_size) {
                case 0x0000 ... 0x3fff:
                    dali_max_mem = 0x3fff;
                break;
                case 0x4000 ... 0x5fff:
                    if (ctx->sfx == SFX_VC20) dali_max_mem = 0x5fff;
                break;
                case 0x6000 ... 0x7fff:
                    if (ctx->sfx == SFX_VC20) dali_max_mem = 0x7fff;
                break;
                default:
            }
            printf("choosing mem config: $%04x\n", dali_max_mem);
            sfx_code[dali_data_end + 4] = dali_max_mem >> 8;
        }

        /* setup jmp target after decompression */
        sfx_code[dali_sfx_addr + 0] = ctx->sfx_addr & 0xff;
        sfx_code[dali_sfx_addr + 1] = ctx->sfx_addr >> 8;

        /* setup decompression destination */
        sfx_code[dali_dst + 0] = ctx->cbm_orig_addr & 0xff;
        sfx_code[dali_dst + 1] = ctx->cbm_orig_addr >> 8;

        /* setup compressed data src */
        sfx_code[dali_src + 0] = ((dali_max_mem & 0xff00) + 0x100 - ctx->reencoded_index) & 0xff;
        sfx_code[dali_src + 1] = ((dali_max_mem & 0xff00) + 0x100 - ctx->reencoded_index) >> 8;

        /* setup compressed data end */
        sfx_code[dali_data_end + 0] = dali_data_end_addr & 0xff;
        sfx_code[dali_data_end + 1] = dali_data_end_addr >> 8;

        /* setup position for zp_code */
        sfx_code[dali_sfx_src + 0] = dali_sfx_src_addr & 255;
        sfx_code[dali_sfx_src + 1] = dali_sfx_src_addr >> 8;

        /* setup value for highbyte comparision */
        sfx_code[dali_data_size_hi] = sfx_code[dali_src + 1] - 1;

        if (ctx->sfx_small) {
            if (ctx->sfx_effect) {
            } else {
            }
        } else {
            if (ctx->sfx_01 < 0) ctx->sfx_01 = 0x37;
            sfx_code[dali_01] = ctx->sfx_01;
            if (ctx->sfx_cli) sfx_code[dali_cli] = 0x58;
            if (ctx->sfx_effect) {
            } else {
            }
        }

        printf("original: $%04x-$%04x ($%04x) 100%%\n", (int)ctx->cbm_orig_addr, (int)ctx->cbm_orig_addr + (int)ctx->unpacked_size, (int)ctx->unpacked_size);

        /* write out load address */
        fputc(dali_load_addr & 255, fp);
        fputc(dali_load_addr >> 8, fp);

        /* write out sfx code  */
        if (fwrite(sfx_code_start, sizeof(char), sfx_code_size, fp) != sfx_code_size) {
            fprintf(stderr, "Error: Cannot write output file %s\n", ctx->output_name);
             exit(1);
        }
        printf("packed:   $%04x-$%04x ($%04x) %3.2f%%\n", dali_load_addr, dali_load_addr + (int)sfx_size + (int)ctx->packed_index, (int)sfx_size + (int)ctx->packed_index, ((float)(ctx->packed_index + (int)sfx_size) / (float)(ctx->unpacked_size) * 100.0));
    /* or standard compressed */
    } else {
        if (ctx->cbm_relocate_origin_addr >= 0 && !ctx->cbm) {
            ctx->cbm_orig_addr = ctx->cbm_relocate_origin_addr;
            ctx->cbm = TRUE;
        }
        if (ctx->cbm_relocate_packed_addr >= 0 || ctx->cbm_relocate_end_addr >= 0) {
            if (ctx->inplace) {
                before_reloc = ctx->cbm_range_to - ctx->packed_index - 2;
            } else {
                before_reloc = ctx->cbm_range_to - 2 - (ctx->unpacked_index - (ctx->unpacked_size - ctx->packed_size));
            }
            if (ctx->cbm_relocate_end_addr >= 0) {
                ctx->cbm_packed_addr = ctx->cbm_relocate_end_addr - ctx->packed_index - 2;
            } else {
                ctx->cbm_packed_addr = ctx->cbm_relocate_packed_addr;
            }
        } else {
            if (ctx->inplace) {
                if (ctx->cbm_relocate_origin_addr >= 0) {
                    ctx->cbm_packed_addr = ctx->cbm_relocate_origin_addr + (ctx->cbm_range_to - ctx->cbm_range_from) - ctx->packed_index - 2;
                } else {
                    /* as we chose inplace, the whole packed stream lengths defines the load address */
                    ctx->cbm_packed_addr = ctx->cbm_range_to - ctx->packed_index - 2;
                }
            } else {
                //ctx->cbm_packed_addr = ctx->cbm_orig_addr;
                /* also include the overlap into the optimal load address */
                ctx->cbm_packed_addr = ctx->cbm_range_to - 2 - (ctx->unpacked_index - (ctx->unpacked_size - ctx->packed_size));
            }
        }

        if (ctx->cbm) {
            printf("original: $%04x-$%04x ($%04x) 100%%\n", (int)ctx->cbm_orig_addr, (int)ctx->cbm_orig_addr + (int)ctx->unpacked_size, (int)ctx->unpacked_size);
            //now relocate result to new address
            if (ctx->cbm_relocate_origin_addr >= 0 && ctx->cbm) {
                ctx->cbm_orig_addr = ctx->cbm_relocate_origin_addr;
            }
            if (before_reloc >= 0 && ctx->inplace) {
                printf("packed:   $%04x-$%04x ($%04x) %3.2f%%\n", (int)before_reloc, (int)before_reloc + (int)ctx->packed_index + 2, (int)ctx->packed_index + 2, ((float)(ctx->packed_index) / (float)(ctx->unpacked_size) * 100.0));
                printf("reloc:    $%04x-$%04x ($%04x) %3.2f%%\n", (int)ctx->cbm_packed_addr, (int)ctx->cbm_packed_addr + (int)ctx->packed_index + 2, (int)ctx->packed_index + 2, ((float)(ctx->packed_index) / (float)(ctx->unpacked_size) * 100.0));
            } else {
                printf("packed:   $%04x-$%04x ($%04x) %3.2f%%\n", (int)ctx->cbm_packed_addr, (int)ctx->cbm_packed_addr + (int)ctx->packed_index + 2, (int)ctx->packed_index + 2, ((float)(ctx->packed_index) / (float)(ctx->unpacked_size) * 100.0));
            }
            if ((ctx->cbm_packed_addr >= 0xd000 && ctx->cbm_packed_addr < 0xe000) || (ctx->cbm_packed_addr < 0xd000 && ctx->cbm_packed_addr + ctx->packed_index + 2 > 0xd000)) {
                fprintf(stderr, "Warning: Packed file lies in I/O-range from $d000-$dfff\n");
                if (ctx->exit_on_warn) exit(1);
            }

            /* little endian */
            file_write_byte(ctx->cbm_packed_addr & 255, fp);
            file_write_byte((ctx->cbm_packed_addr >> 8) & 255, fp);

            /* little endian */
            file_write_byte(ctx->cbm_orig_addr & 255, fp);
            file_write_byte((ctx->cbm_orig_addr >> 8) & 255, fp);
        } else {
            printf("original: $%04x-$%04x ($%04x) 100%%\n", 0, (int)ctx->unpacked_size, (int)ctx->unpacked_size);
            printf("packed:   $%04x-$%04x ($%04x) %3.2f%%\n", 0, (int)ctx->packed_index, (int)ctx->packed_index, ((float)(ctx->packed_index) / (float)(ctx->unpacked_size) * 100.0));
        }
    }

    /* write out compressed data, either stand alone, or appended to previously written sfx code */
    if (fwrite(ctx->reencoded_data, sizeof(char), ctx->packed_index, fp) != ctx->packed_index) {
        fprintf(stderr, "Error: Cannot write output file\n");
        exit(1);
    }
    fclose(fp);
}

void do_reencode(ctx* ctx) {
    char tmp_name[] = "dict-XXXXXX";
    char src_name[] = "src-XXXXXX";
    unsigned char *dict_data = NULL;
    int dict_size = 0;
    char *salvador_argv[5];
    int salvador_argc = 0;
    int dict_temp = FALSE;
    FILE *dfp = NULL;
    FILE *sfp = NULL;
    FILE* ufp = NULL;
    FILE* pfp = NULL;


    /* determine output filename */
    if (ctx->output_name == NULL) {
        ctx->output_name = (char *)malloc(strlen(ctx->input_name) + 4);
        strcpy(ctx->output_name, ctx->input_name);
        strcat(ctx->output_name, ".lz");
        printf("output name: %s\n", ctx->output_name);
    }

    /* allocate buffers */
    ctx->packed_data = (unsigned char *)malloc(BUFFER_SIZE);
    ctx->unpacked_data = (unsigned char *)malloc(BUFFER_SIZE + 2);
    ctx->reencoded_data = (unsigned char *)malloc(BUFFER_SIZE);

    if (!ctx->packed_data || !ctx->unpacked_data || !ctx->reencoded_data) {
        fprintf(stderr, "Error: Insufficient memory\n");
        exit(1);
    }

    /* load unpacked file */
    ufp = fopen(ctx->input_name, "rb");
    if (!ufp) {
        fprintf(stderr, "Error: Cannot access input file\n");
        exit(1);
    }
    ctx->unpacked_size = fread(ctx->unpacked_data, sizeof(char), BUFFER_SIZE + 2, ufp);
    fclose(ufp);

    /* cbm address handling */
    if (ctx->cbm_relocate_origin_addr >= 0 && !ctx->cbm) {
        ctx->cbm_orig_addr = ctx->cbm_relocate_origin_addr;
    } else if (!ctx->cbm) {
        ctx->cbm_orig_addr = 0;
    } else {
        ctx->cbm_orig_addr = ctx->unpacked_data[0] + (ctx->unpacked_data[1] << 8);
    }

    if (ctx->cbm) {
      ctx->unpacked_data += 2;
      ctx->unpacked_size -= 2;
    }

    /* take care of range (--from --to) */
    if (ctx->cbm_range_from < 0) ctx->cbm_range_from = ctx->cbm_orig_addr;
    if (ctx->cbm_range_to < 0) ctx->cbm_range_to = ctx->cbm_orig_addr + ctx->unpacked_size;

    if ((ctx->cbm_range_to - ctx->cbm_orig_addr) > ctx->unpacked_size) {
        ctx->cbm_range_to = ctx->unpacked_size + ctx->cbm_orig_addr;
        fprintf(stderr, "Warning: File ends at $%04x, adopting --to value\n", ctx->cbm_range_to);
    }
    ctx->unpacked_size = (ctx->cbm_range_to - ctx->cbm_orig_addr);

    /* if range is below start_address, adopt range */
    if (ctx->cbm_range_from < ctx->cbm_orig_addr) {
        ctx->cbm_range_from = ctx->cbm_orig_addr;
        fprintf(stderr, "Warning: File starts at $%04x, adopting --from value\n", ctx->cbm_range_from);
    }
    if (ctx->cbm_range_from > ctx->cbm_range_to) {
        fprintf(stderr, "Error: --from beyond fileend ($%04x - $%04x)\n", ctx->cbm_range_from, ctx->cbm_range_to);
        exit(1);
    }

    /* setup dict lengths and position */
    if (ctx->cbm_prefix_from >= 0) {
        if (ctx->cbm_range_from < 0) {
            fprintf(stderr, "Error: Dict is zero size (use --from)\n");
            exit(1);
        }
        else if (ctx->cbm_prefix_from >= ctx->cbm_range_from) {
            fprintf(stderr, "Error: --from must be greater than --prefix-from\n");
            exit(1);
        }
        if (ctx->cbm_range_from >= 0 && ctx->cbm_range_from - ctx->cbm_prefix_from > 32640) {
            //ctx->cbm_prefix_from = ctx->cbm_range_from - 32640;
            fprintf(stderr, "Info: --prefix-from  exceeds max offset, not all bytes can be used\n");
        }
        /* if range is below start_address, adopt range */
        if (ctx->cbm_prefix_from < ctx->cbm_orig_addr) {
            ctx->cbm_prefix_from = ctx->cbm_orig_addr;
            fprintf(stderr, "Warning: File starts at $%04x, adopting --prefix-from value\n", ctx->cbm_prefix_from);
        }
        dict_data = ctx->unpacked_data + ctx->cbm_prefix_from - ctx->cbm_orig_addr;
        dict_size = ctx->cbm_range_from - ctx->cbm_prefix_from;
    }

    /* load file from start_pos on only, so skip bytes on input */
    ctx->unpacked_data += (ctx->cbm_range_from - ctx->cbm_orig_addr);
    /* also adopt ctx->unpacked_size */
    ctx->unpacked_size -= (ctx->cbm_range_from - ctx->cbm_orig_addr);
    /* and set up new load-address */
    ctx->cbm_orig_addr = ctx->cbm_range_from;

    if (ctx->unpacked_size <= 0) {
        fprintf(stderr, "Error: Input too small\n");
        exit(1);
    }

    printf("Compressing from $%04x to $%04x = $%04x bytes\n", (int)ctx->cbm_range_from, (int)ctx->cbm_range_to, (int)ctx->unpacked_size);

    /* write clamped raw data */
    ctx->clamped_name = (char*)malloc(sizeof(src_name));
    strcpy(ctx->clamped_name, src_name);
//#ifdef _WIN32
//    sfp = fopen(mktemp(ctx->clamped_name),"wb");
//#else
    sfp = fdopen(mkstemp(ctx->clamped_name),"wb");
//#endif
    if (!sfp) {
        fprintf(stderr, "Error: Cannot create clamped file %s\n", ctx->clamped_name);
        exit(1);
    }
    if (ctx->unpacked_size != 0) {
        if (fwrite(ctx->unpacked_data, sizeof(char), ctx->unpacked_size, sfp) != ctx->unpacked_size) {
            fprintf(stderr, "Error: Cannot write clamped file\n");
            remove(ctx->clamped_name);
            exit(1);
        }
    }
    fclose(sfp);

    /* ctreate temp file for dict */
    if (ctx->cbm_prefix_from >= 0) {
        if (ctx->prefix_name == NULL) {
            ctx->prefix_name = (char*)malloc(sizeof(tmp_name));
            strcpy(ctx->prefix_name, tmp_name);
//#ifdef _WIN32
//            dfp = fopen(mktemp(ctx->prefix_name),"wb");
//#else
            dfp = fdopen(mkstemp(ctx->prefix_name),"wb");
//#endif
            printf("using prefix: $%04x - $%04x\n", ctx->cbm_prefix_from, ctx->cbm_prefix_from + dict_size);
            if (!dfp) {
                fprintf(stderr, "Error: Cannot create dict file %s\n", ctx->prefix_name);
                exit(1);
            }
            if (!dict_data || fwrite(dict_data, sizeof(char), dict_size, dfp) != dict_size) {
                fprintf(stderr, "Error: Cannot write dict file %s\n", ctx->prefix_name);
                remove(ctx->prefix_name);
                exit(1);
            }
            dict_temp = TRUE;
            fclose(dfp);
        }
    }

    /* compress data with salvador */
    salvador_argv[salvador_argc++] = "salvador";
    if (ctx->prefix_name) {
        salvador_argv[salvador_argc++] = "-D";
        salvador_argv[salvador_argc++] = ctx->prefix_name;
    }
    salvador_argv[salvador_argc++] = ctx->clamped_name;
    salvador_argv[salvador_argc++] = ctx->output_name;
    salvador_main(salvador_argc, salvador_argv);

    /* delete dict */
    if (dict_temp) remove(ctx->prefix_name);
    /* remove clamped */
    remove(ctx->clamped_name);

    /* read packed data */
    pfp = fopen(ctx->output_name, "rb");
    if (!pfp) {
        fprintf(stderr, "Error: Cannot access input file\n");
        exit(1);
    }
    ctx->packed_size = fread(ctx->packed_data, sizeof(char), BUFFER_SIZE, pfp);
    fclose(pfp);

    /* determine size without eof-marker -> remove 18 bits, either 2 byte or three byte depending on position of last bitpair */
    if (ctx->packed_data[ctx->packed_size - 1] & 0x80) ctx->packed_size -= 3;
    else ctx->packed_size -= 2;

    reencode_packed_stream(ctx);

    if (ctx->packed_index + 2 > ctx->unpacked_size) {
        fprintf(stderr, "Error: Packed file larger than original\n");
        exit(1);
    }

    //printf("control-bytes: $%04x\n", ctx->lz_bits);

    write_reencoded_stream(ctx);
    return;
}

int main(int argc, char *argv[]) {
    int i;
    int show_version = FALSE;
    int exit_help = 1;

    ctx ctx = { 0 };

    ctx.output_name = NULL;
    ctx.input_name = NULL;
    ctx.prefix_name = NULL;

    ctx.inplace = -1;

    ctx.cbm = TRUE;
    ctx.cbm_orig_addr = 0;
    ctx.cbm_packed_addr = 0;
    ctx.cbm_range_from = -1;
    ctx.cbm_range_to = -1;
    ctx.cbm_relocate_packed_addr = -1;
    ctx.cbm_relocate_end_addr = - 1;
    ctx.cbm_relocate_origin_addr = -1;
    ctx.cbm_relocate_sfx_addr = -1;
    ctx.cbm_prefix_from = -1;
    ctx.force_inplace = FALSE;

    ctx.sfx = 0;
    ctx.sfx_addr = -1;
    ctx.sfx_01 = -1;
    ctx.sfx_cli = FALSE;
    ctx.sfx_small = FALSE;
    ctx.sfx_effect = FALSE;
    ctx.exit_on_warn = FALSE;

    for (i = 1; i < argc; i++) {
        if (!strncmp(argv[i], "-", 1) || !strncmp(argv[i], "--", 2)) {
            if (!strcmp(argv[i], "--binfile")) {
                ctx.cbm = FALSE;
            } else if (!strcmp(argv[i], "--prefix-from")) {
                ctx.cbm_prefix_from = read_number(argv[i + 1], argv[i], 65536);
                i++;
            } else if (!strcmp(argv[i], "--prefix-file")) {
                i++;
                ctx.prefix_name = argv[i];
            } else if (!strcmp(argv[i], "--version") || !strcmp(argv[i], "-V")) {
                show_version = TRUE;
            } else if (!strcmp(argv[i], "--help") || !strcmp(argv[i], "-h")) {
                exit_help = 0;
            } else if (!strcmp(argv[i], "--exit-on-warn")) {
                ctx.exit_on_warn = TRUE;
            } else if (!strcmp(argv[i], "--no-inplace")) {
                ctx.inplace = FALSE;
            } else if (!strcmp(argv[i], "--small")) {
                ctx.sfx_small = TRUE;
            } else if (!strcmp(argv[i], "--effect")) {
                ctx.sfx_effect = TRUE;
            } else if (!strcmp(argv[i], "--inplace")) {
                ctx.inplace = TRUE;
            } else if (!strcmp(argv[i], "--relocate-end")) {
                ctx.cbm_relocate_end_addr = read_number(argv[i + 1], argv[i], 65536);
                i++;
            } else if (!strcmp(argv[i], "--relocate-packed")) {
                ctx.cbm_relocate_packed_addr = read_number(argv[i + 1], argv[i], 65536);
                i++;
            } else if (!strcmp(argv[i], "--relocate-origin")) {
                ctx.cbm_relocate_origin_addr = read_number(argv[i + 1], argv[i], 65536);
                i++;
            } else if (!strcmp(argv[i], "--relocate-sfx")) {
                ctx.cbm_relocate_sfx_addr = read_number(argv[i + 1], argv[i], 65536);
                i++;
            } else if (!strcmp(argv[i], "--from")) {
                ctx.cbm_range_from = read_number(argv[i + 1], argv[i], 65536);
                i++;
            } else if (!strcmp(argv[i], "--to")) {
                ctx.cbm_range_to = read_number(argv[i + 1], argv[i], 65536);
                i++;
            } else if (!strcmp(argv[i], "--01")) {
                ctx.sfx_01 = read_number(argv[i + 1], argv[i], 256);
                i++;
            } else if (!strcmp(argv[i], "--cli")) {
                ctx.sfx_cli = TRUE;
            } else if (!strcmp(argv[i], "--sfx")) {
                if (ctx.sfx) fprintf(stderr, "Error: Multiple sfx-types given %s\n", argv[i]);
                ctx.sfx_addr = read_number(argv[i + 1], argv[i], 65536);
                i++;
                ctx.sfx = SFX_C64;
            } else if (!strcmp(argv[i], "--sfx_plus4")) {
                if (ctx.sfx) fprintf(stderr, "Error: Multiple sfx-types given %s\n", argv[i]);
                ctx.sfx_addr = read_number(argv[i + 1], argv[i], 65536);
                i++;
                ctx.sfx = SFX_PLUS4;
            } else if (!strcmp(argv[i], "--sfx_vc20")) {
                if (ctx.sfx) fprintf(stderr, "Error: Multiple sfx-types given %s\n", argv[i]);
                ctx.sfx_addr = read_number(argv[i + 1], argv[i], 65536);
                i++;
                ctx.sfx = SFX_VC20;
            } else if (!strcmp(argv[i], "-o")) {
                i++;
                ctx.output_name = argv[i];
            } else {
                fprintf(stderr, "Error: Unknown option %s\n", argv[i]);
                exit(1);
            }
        } else if (i == argc - 1) {
            ctx.input_name = argv[i];
        } else {
            fprintf(stderr, "Error: Unknown option %s\n", argv[i]);
            exit(1);
        }
    }

    printf("dali v0.3.5 - a zx0-reencoder for bitfire by Tobias Bindhammer\n");
    printf("underlying zx0-packer salvador by Emmanuel Marty\n");

    if (argc <= 2 && show_version) exit(0);
    if (argc == 1 || !exit_help) {
        fprintf(stderr, "Usage: %s [options] input\n"
                        "  -o [filename]               Set output filename.\n"
                        "  --sfx [num]                 Create a c64 compatible sfx-executable.\n"
                        "  --sfx_plus4 [num]           Create a plus4 compatible sfx-executable.\n"
                        "  --sfx_vc20 [num]            Create a vc20 compatible sfx-executable.\n"
                        "  --01 [num]                  Set 01 to [num] after sfx. (c64 sfx only)\n"
                        "  --cli [num]                 Do a CLI after sfx, default is SEI. (c64 sfx only)\n"
                        "  --effect                    A very simple decrunch effect is applied. (c64 sfx only)\n"
                        "  --small                     Use a very small depacker that fits into zeropage, but --01 and --cli are ignored and it trashes zeropage (!) (c64 sfx only)\n"
                        "  --inplace                   Explicitely enable inplace-decompression (overwrites default).\n"
                        "  --no-inplace                Explicitely disable inplace-decompression (overwrites default).\n"
                        "  --binfile                   Input file is a raw binary without load-address.\n"
                        "  --from [num]                Compress file from [num] on.\n"
                        "  --to [num]                  Compress file until position [num].\n"
                        "  --prefix-from [num]         Use preceeding data from [num] on as dictionary (in combination with --from).\n"
                        "  --prefix-file [file]        Use preceeding data from [file] as dictionary.\n"
                        "  --relocate-packed [num]     Relocate packed data to desired address [num] (resulting file can't de decompressed inplace!)\n"
                        "  --relocate-end [num]        Relocate packed data so it ends at desired address [num] (resulting file can't de decompressed inplace!)\n"
                        "  --relocate-origin [num]     Set load-address of source file/selected chunk to [num] prior to compression. If used on bin-files, load-address and depack-target is prepended on output.\n"
                        "  --relocate-sfx [num]        Set load-address sfx-packed file to [num], basic header will then be omitted.\n"
                        "  --exit-on-warn              Exit on warnings like they happen when crossing the i/o-range.\n"
                        "  -h, --help                  Show this help page.\n"
                        "  -V, --version               Show version info and exit.\n"
                        ,argv[0]);
        if (argc <= (2 + show_version)) exit(exit_help);
    }

    if (ctx.cbm_relocate_packed_addr >= 0 && ctx.cbm_relocate_end_addr >= 0) {
        fprintf(stderr, "Error: --relocate-packed and --relocate-end is set, choose either one\n");
        exit(1);
    }
    if (!ctx.sfx && ctx.cbm_relocate_sfx_addr >= 0) {
        fprintf(stderr, "Info: No sfx, ignoring --relocate-sfx option\n");
    }
    if (!ctx.sfx && ctx.sfx_small) {
        fprintf(stderr, "Info: No sfx, ignoring --small option\n");
    }
    if (!ctx.sfx && ctx.sfx_01 >= 0) {
        fprintf(stderr, "Info: No sfx, ignoring --01 option\n");
    }
    if (ctx.sfx > SFX_C64 && ctx.sfx_01 >= 0) {
        fprintf(stderr, "Info: ignoring --01 option for plus4/vc20 sfx\n");
    }
    if (ctx.sfx > SFX_C64 && ctx.sfx_cli) {
        fprintf(stderr, "Info: ignoring --cli option for plus4/vc20 sfx\n");
    }
    if (ctx.sfx > SFX_C64 && ctx.sfx_small) {
        fprintf(stderr, "Info: ignoring --small option for plus4/vc20 sfx\n");
    }
    if (ctx.sfx > SFX_C64 && ctx.sfx_effect) {
        fprintf(stderr, "Info: ignoring --effect option for plus4/vc20 sfx\n");
    }
    if (!ctx.sfx && ctx.sfx_cli) {
        fprintf(stderr, "Info: No sfx, ignoring --cli option\n");
    }

    if (ctx.input_name == NULL) {
        fprintf(stderr, "Error: No input-filename given\n");
        exit(1);
    }

    if (ctx.sfx) {
        ctx.inplace = FALSE;
    }
    if (ctx.inplace < 0) {
        if (ctx.cbm_relocate_packed_addr >= 0 || ctx.cbm_relocate_end_addr >= 0) {
            ctx.inplace = FALSE;
        } else {
            ctx.inplace = TRUE;
        }
    }

    do_reencode(&ctx);
    return 0;
}
