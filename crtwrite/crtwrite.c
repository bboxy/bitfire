/*
 * (c) Copyright 2021 by Tobias Bindhammer. All rights reserved.
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

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include <png.h>

#ifdef _MSC_VER
#else
	#include <libgen.h>
#endif

#include "debug.h"
#include "bootstrap.h"

int crt_write_file(FILE* crt, char* file, int header) {
    FILE* fp;
    int c;
    int addr;
    int size = 0;

    fp = fopen(file, "rb");
    if (!fp) {
        fatal_message("can't open file '%s'\n", file);
    }
    fseek(fp, 0L, SEEK_END);
    size = ftell(fp) - 2;
    rewind(fp);

    addr = fgetc(fp);
    addr |= (fgetc(fp) << 8);

    if (header) {
        fputc((addr & 255), crt);
        fputc((addr >> 8),  crt);

        fputc((size & 255), crt);
        fputc((size >> 8),  crt);
    }

    while ((c = fgetc(fp)) != EOF) {
        fputc(c, crt);
    }
    fclose(fp);
    return size + (header * 4);
}

int main(int argc, char *argv[]) {
    int c;
    FILE* crt;
    int crt_pos = 0;
    char* crt_path = NULL;
    int done = 0;

    if (argc <= 1) {
        printf("Usage: crtwrite -o image.bin [-b file]\n");
        exit (0);
    }

    c = 0;
    while(++c < argc) {
        if(!strcmp(argv[c], "-o")) {
            if (crt_path) {
                fatal_message("image name already given ('%s')\n", crt_path);
            }
            if (argc -c > 1) {
                crt_path = argv[++c];
            } else {
                fatal_message("missing path for option '%s'\n", argv[c]);
            }
        }
        else if(!strcmp(argv[c], "-b")) {
            c++;
        }
        else {
            fatal_message("unknown option '%s'\n", argv[c]);
        }
    }

    crt = fopen(crt_path, "wb");

    for (c = 2; c < sizeof(bootstrap); c++) {
        fputc(bootstrap[c],crt);
        crt_pos++;
    }

    c = 0;
    while(++c < argc) {
        if(argc -c > 1 && (!strcmp(argv[c], "-b") || !strcmp(argv[c], "--boot"))) {
            printf("$%04x: %s\n",crt_pos,argv[c + 1]);
            crt_pos += crt_write_file(crt, argv[++c], 1);
            if (!done) {
                while ((crt_pos & 255) != 0) {
                    fputc(0xff,crt);
                    crt_pos++;
                }
                done = 1;
            }
        }
    }

    while (crt_pos < 0x100000) {
        fputc(0xff, crt);
        crt_pos++;
    }
    fclose(crt);
    exit(0);
}

