/*
 * Modified version (c) Copyright 2021 by Tobias Bindhammer. All rights reserved.
 *
 * Based on original (c) Copyright 2021 by Einar Saukas. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * The name of its author may not be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "zx0.h"
#include "compress.h"
#include "optimize.h"
#include "sfx.h"

#define MAX_OFFSET_ZX0    32640
#define MAX_OFFSET_ZX7     2176

static void reverse(unsigned char *first, unsigned char *last) {
    unsigned char c;

    while (first < last) {
        c = *first;
        *first++ = *last;
        *last-- = c;
    }
}

static void file_write_byte(int byte, FILE *ofp, char* output_name) {
    if (fputc(byte, ofp) != byte) {
        fprintf(stderr, "Error: Cannot write output file %s\n", output_name);
        exit(1);
    }
    return;
}

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

int main(int argc, char *argv[]) {
    int skip = 0;
    int forced_mode = FALSE;
    int quick_mode = FALSE;
    int backwards_mode = FALSE;
    char *output_name = NULL;
    char *input_name = NULL;
    unsigned char *input_data;
    unsigned char *output_data;
    FILE *ifp;
    FILE *ofp;
    int input_size;
    int output_size;
    int partial_counter;
    int total_counter;
    int delta;
    int i;
    int inplace = TRUE;
    int cbm = TRUE;
    int inplace_end_pos = -1;
    int cbm_orig_addr = 0;
    int cbm_packed_addr = 0;
    int cbm_use_prefix = FALSE;
    int cbm_range_start = -1;
    int cbm_range_end = -1;
    int cbm_relocate_packed_addr = -1;
    int cbm_relocate_origin_addr = -1;
    int cbm_result_size = 0;
    int file_start_pos = 0;
    int sfx = FALSE;
    int sfx_addr = -1;
    char *sfx_code = NULL;

    printf("Based on ZX0 v1.5 by Einar Saukas\n");
    printf("Ported and adopted for C64 by Tobias Bindhammer\n");
    printf("Compression speed optimization by Soci/Singular\n");

    //XXX TODO use prefix also with range? -> from to --prefix size? from to gibt prefix + file an? give a filename for prefix?

    /* process hidden optional parameters */
    for (i = 1; i < argc; i++) {
        if (!strncmp(argv[i], "-", 1) || !strncmp(argv[i], "--", 2)) {
            if (!strcmp(argv[i], "-f")) {
                forced_mode = TRUE;
            } else if (!strcmp(argv[i], "--binfile")) {
                cbm = FALSE;
            } else if (!strcmp(argv[i], "--use-prefix")) {
                cbm_use_prefix = TRUE;
            } else if (!strcmp(argv[i], "--no-inplace")) {
                inplace = FALSE;
            } else if (!strcmp(argv[i], "--relocate-packed")) {
                i++;
                cbm_relocate_packed_addr = read_number(argv[i]);
            } else if (!strcmp(argv[i], "--relocate-origin")) {
                i++;
                cbm_relocate_origin_addr = read_number(argv[i]);
            } else if (!strcmp(argv[i], "--from")) {
                i++;
                cbm_range_start = read_number(argv[i]);
            } else if (!strcmp(argv[i], "--to")) {
                i++;
                cbm_range_end = read_number(argv[i]);
            } else if (!strcmp(argv[i], "--sfx")) {
                i++;
                sfx_addr = read_number(argv[i]);
                inplace = FALSE;
                sfx = TRUE;
            } else if (!strcmp(argv[i], "-o")) {
                i++;
                output_name = argv[i];
            } else if (!strcmp(argv[i], "-q")) {
                quick_mode = TRUE;
            } else if (!strcmp(argv[i], "-b")) {
                backwards_mode = TRUE;
            } else {
                fprintf(stderr, "Error: Unknown option %s\n", argv[i]);
                exit(1);
            }
        } else if (i == argc - 1) {
            input_name = argv[i];
        } else {
            fprintf(stderr, "Error: Uknown option %s\n", argv[i]);
            exit(1);
        }
    }

    if (argc == 1) {
        fprintf(stderr, "Usage: %s [options] input\n"
                        "  -f                         Force overwrite of output file\n"
                        "  -o [filename]              Set output filename\n"
                        "  -q                         Quick non-optimal compression\n"
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
        output_name = (char *)malloc(strlen(input_name)+4);
        strcpy(output_name, input_name);
        strcat(output_name, ".lz");
    }

    /* open input file */
    ifp = fopen(input_name, "rb");
    if (!ifp) {
        fprintf(stderr, "Error: Cannot access input file %s\n", input_name);
        exit(1);
    }

    /* get full file size */
    fseek(ifp, 0L, SEEK_END);
    input_size = ftell(ifp);
    fseek(ifp, 0L, SEEK_SET);

    if (cbm && input_size >= 2) {
        cbm_orig_addr = fgetc(ifp) & 255;
        cbm_orig_addr |= (fgetc(ifp) & 255) << 8;
        file_start_pos += 2;
        input_size -= 2;
    }

    if (!input_size) {
        fprintf(stderr, "Error: Empty input file %s\n", argv[i]);
        exit(1);
    }

    if (cbm_range_end >= 0) {
        if ((cbm_range_end - cbm_orig_addr) > input_size) {
            cbm_range_end = input_size + cbm_orig_addr;
            fprintf(stderr, "Warning: File ends at $%04x, adopting --to value\n", cbm_range_end);
        }
        input_size = (cbm_range_end - cbm_orig_addr);
    }

    if (cbm_range_start >= 0) {
        /* if range is below start_address, adopt range */
        if (cbm_range_start < cbm_orig_addr) {
            cbm_range_start = cbm_orig_addr;
            fprintf(stderr, "Warning: File starts at $%04x, adopting --from value\n", cbm_range_start);
        }
        if (cbm_use_prefix) {
            /* load full file, but skip until start_pos */
            skip = cbm_range_start - cbm_orig_addr;
        } else {
            /* load file from start_pos on only, so skip bytes on input */
            file_start_pos += (cbm_range_start - cbm_orig_addr);
            /* also adopt input_size */
            input_size -= (cbm_range_start - cbm_orig_addr);
        }
        cbm_orig_addr = cbm_range_start;
    }

    if (cbm_range_end >= 0 && cbm_range_start >= 0) {
        if (cbm_range_start > cbm_range_end) {
            fprintf(stderr, "Error: --from is bigger than --to\n");
            exit(1);
        }
    }

    /* load bytes from start_pos on */
    fseek(ifp, file_start_pos, SEEK_SET);

    if (cbm_relocate_origin_addr >= 0) {
        cbm_orig_addr = cbm_relocate_origin_addr;
        cbm = TRUE;
    }

    if (cbm_relocate_packed_addr >= 0 || sfx) {
        inplace = FALSE;
    }

    /* validate skip against input size */
    if (skip >= input_size) {
        fprintf(stderr, "Error: Skipping entire input file %s\n", argv[i]);
        exit(1);
    }

    /* allocate input buffer */
    input_data = (unsigned char *)malloc(input_size);
    if (!input_data) {
        fprintf(stderr, "Error: Insufficient memory\n");
        exit(1);
    }

    /* read input file */
    total_counter = 0;
    do {
        partial_counter = fread(input_data+total_counter, sizeof(char), input_size-total_counter, ifp);
        total_counter += partial_counter;
    } while (partial_counter > 0);

    if (total_counter != input_size) {
        fprintf(stderr, "Error: Cannot read input file %s\n", argv[i]);
        exit(1);
    }

    /* close input file */
    fclose(ifp);

    /* check output file */
    if (!forced_mode && fopen(output_name, "rb") != NULL) {
        fprintf(stderr, "Error: Already existing output file %s\n", output_name);
        exit(1);
    }

    /* create output file */
    ofp = fopen(output_name, "wb");
    if (!ofp) {
        fprintf(stderr, "Error: Cannot create output file %s\n", output_name);
        exit(1);
    }

    /* conditionally reverse input file */
    if (backwards_mode)
        reverse(input_data, input_data+input_size-1);

    if (sfx)
        printf("Creating sfx with start-address $%04x\n", sfx_addr);

    /* generate output file */
    output_data = compress(optimize(input_data, input_size, skip, quick_mode ? MAX_OFFSET_ZX7 : MAX_OFFSET_ZX0), input_data, input_size, skip, backwards_mode, &output_size, &delta, inplace, &inplace_end_pos);

    /* conditionally reverse output file */
    if (backwards_mode)
        reverse(output_data, output_data+output_size-1);

    if (cbm && !sfx) {
        // XXX TODO check if relocated packed addr >= cbm_packed_addr and if relocated_packed_addr + inplace_end_pos <= cbm_orig_addr)
        cbm_result_size = output_size + 2;// + 2;
        cbm_packed_addr = cbm_orig_addr + input_size - skip - cbm_result_size;

        if (cbm_relocate_packed_addr >= 0) {
            if ((cbm_relocate_packed_addr <= cbm_packed_addr) && (cbm_relocate_packed_addr + cbm_result_size >= cbm_orig_addr)) {
                fprintf(stderr, "Error: File can't be safely decompressed at $%04x\n", cbm_relocate_packed_addr);
                exit(1);
            }
            cbm_packed_addr = cbm_relocate_packed_addr;
        }

        /* measure end_pos from lz_dst on */
        inplace_end_pos += cbm_orig_addr;

        /* little endian */
        file_write_byte(cbm_packed_addr & 255, ofp, output_name);
        file_write_byte((cbm_packed_addr >> 8) & 255, ofp, output_name);
        /* big endian, as read backwards by depacker */
        file_write_byte((cbm_orig_addr >> 8) & 255, ofp, output_name);
        file_write_byte(cbm_orig_addr & 255, ofp, output_name);

        printf("original:     $%04x-$%04x ($%04x)\n", cbm_orig_addr, cbm_orig_addr + input_size - skip, input_size - skip);
        printf("packed:       $%04x-$%04x ($%04x)   % 3.2f%% saved\n", cbm_packed_addr, cbm_packed_addr + cbm_result_size, cbm_result_size, ((float)(input_size - skip - cbm_result_size) / (float)(input_size - skip) * 100.0));

        //if (inplace) {
        //    /* big endian, as read backwards by depacker */
        //    file_write_byte(((inplace_end_pos) >> 8) & 255, ofp, output_name);
        //    file_write_byte((inplace_end_pos) & 255, ofp, output_name);
        //} else {
        //    /* disable inplace end-check */
        //    file_write_byte(0, ofp, output_name);
        //    file_write_byte(0, ofp, output_name);
        //}
    }

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
        sfx_code[ZX0_SRC + 0] = (0x10000 - output_size) & 0xff;
        sfx_code[ZX0_SRC + 1] = ((0x10000 - output_size) >> 8) & 0xff;
        //sfx_code[ZX0_SRC_LO  + 0] = (0x10000 - output_size) & 0xff;
        //sfx_code[ZX0_SRC_HI1 + 1] = ((0x10000 - output_size) >> 8) & 0xff;
        //sfx_code[ZX0_SRC_HI2 + 1] = ((0x10000 - output_size) >> 8) & 0xff;
        //sfx_code[ZX0_SRC_HI3 + 1] = ((0x10000 - output_size) >> 8) & 0xff;

        /* setup compressed data end */
        sfx_code[ZX0_DATA_END + 0] = (0x0801 + sizeof(decruncher) - 2 + output_size - 0x100) & 0xff;
        sfx_code[ZX0_DATA_END + 1] = ((0x0801 + sizeof(decruncher) - 2 + output_size - 0x100) >> 8) & 0xff;

        sfx_code[ZX0_DATA_SIZE_HI] = ((output_size + 0x100) >> 8) & 0xff;

        if (fwrite(sfx_code, sizeof(char), sizeof(decruncher), ofp) != sizeof(decruncher)) {
            fprintf(stderr, "Error: Cannot write output file %s\n", output_name);
            exit(1);
        }

        printf("original:     $%04x-$%04x ($%04x)\n", cbm_orig_addr, cbm_orig_addr + input_size - skip, input_size - skip);
        printf("packed:       $%04x-$%04x ($%04x)   % 3.2f%% saved\n", 0x0801, 0x0801 + (int)sizeof(decruncher) + output_size, (int)sizeof(decruncher) + output_size, ((float)(input_size - skip - output_size - (int)sizeof(decruncher)) / (float)(input_size - skip) * 100.0));
    }

    /* write output file */
    if (fwrite(output_data, sizeof(char), output_size, ofp) != output_size) {
        fprintf(stderr, "Error: Cannot write output file %s\n", output_name);
        exit(1);
    }

    /* close output file */
    fclose(ofp);

    if (cbm) {
        if ((cbm_packed_addr >= 0xd000 && cbm_packed_addr < 0xe000) || (cbm_packed_addr + cbm_result_size > 0xd000 && cbm_packed_addr + cbm_result_size < 0xe000)) {
            fprintf(stderr, "Error: Packed file lies in I/O-range from $d000-$dfff\n");
            exit(1);
        }
    }

    /* done! */
    if (!cbm) {
        printf("File%s compressed%s from %d to %d bytes! (delta %d)\n", (skip ? " partially" : ""), (backwards_mode ? " backwards" : ""), input_size-skip, output_size, delta);
    }

    return 0;
}
