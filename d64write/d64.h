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

#define FILETYPE_DEL	0x00
#define FILETYPE_SEQ	0x01
#define FILETYPE_PRG	0x02
#define FILETYPE_USR	0x03
#define FILETYPE_REL	0x04
#define FILETYPE_DIR	0x05

#define FILETYPE_STANDARD 	0x00
#define FILETYPE_BITFIRE	0x01
#define FILETYPE_BOOT		0x02

#define MAX_C64_NAME            16

static const char filetype[][4] = {
    "DEL",  /* 0x00 */
    "SEQ",  /* 0x01 */
    "PRG",  /* 0x02 */
    "USR",  /* 0x03 */
    "REL",  /* 0x04 */
    "DIR",  /* 0x05 */
};

#define DIR_INTERLEAVE  1
#define FILE_INTERLEAVE 4

#define D64_BAM_TRACK   18
#define D64_BAM_SECTOR  0

#define D64_DIR_TRACK   18
#define D64_DIR_SECTOR  1

#define BAM_FREE        1
#define BAM_USED        0

#define D64_MIN_TRACK   1
#define D64_MAX_TRACK   35

#define SECTOR_SIZE     256
#define BITFIRE_DIRSECT 18

#define D64_DTRACK      0x03
#define D64_DSECTOR     0x04
#define D64_DTYPE       0x02
#define D64_DNAME       0x05
#define D64_DBLOCKS     0x1e

#define D64_BAM_HEADER  0x90
#define D64_BAM_ID      0xa2

static const int sectors[] = {
     0,
    21,    /* track 1 */
    21,    /* track 2 */
    21,    /* track 3 */
    21,    /* track 4 */
    21,    /* track 5 */
    21,    /* track 6 */
    21,    /* track 7 */
    21,    /* track 8 */
    21,    /* track 9 */
    21,    /* track 10 */
    21,    /* track 11 */
    21,    /* track 12 */
    21,    /* track 13 */
    21,    /* track 14 */
    21,    /* track 15 */
    21,    /* track 16 */
    21,    /* track 17 */
    19,    /* track 18 */
    19,    /* track 19 */
    19,    /* track 20 */
    19,    /* track 21 */
    19,    /* track 22 */
    19,    /* track 23 */
    19,    /* track 24 */
    18,    /* track 25 */
    18,    /* track 26 */
    18,    /* track 27 */
    18,    /* track 28 */
    18,    /* track 29 */
    18,    /* track 30 */
    17,    /* track 31 */
    17,    /* track 32 */
    17,    /* track 33 */
    17,    /* track 34 */
    17,    /* track 35 */
    17,    /* track 36 */
    17,    /* track 37 */
    17,    /* track 38 */
    17,    /* track 39 */
    17     /* track 40 */
};

static const unsigned int track_offsets[] = {
    0x00000,
    0x00000,    /* track 1 */
    0x01500,    /* track 2 */
    0x02a00,    /* track 3 */
    0x03f00,    /* track 4 */
    0x05400,    /* track 5 */
    0x06900,    /* track 6 */
    0x07e00,    /* track 7 */
    0x09300,    /* track 8 */
    0x0a800,    /* track 9 */
    0x0bd00,    /* track 10 */
    0x0d200,    /* track 11 */
    0x0e700,    /* track 12 */
    0x0fc00,    /* track 13 */
    0x11100,    /* track 14 */
    0x12600,    /* track 15 */
    0x13b00,    /* track 16 */
    0x15000,    /* track 17 */
    0x16500,    /* track 18 */
    0x17800,    /* track 19 */
    0x18b00,    /* track 20 */
    0x19e00,    /* track 21 */
    0x1b100,    /* track 22 */
    0x1c400,    /* track 23 */
    0x1d700,    /* track 24 */
    0x1ea00,    /* track 25 */
    0x1fc00,    /* track 26 */
    0x20e00,    /* track 27 */
    0x22000,    /* track 28 */
    0x23200,    /* track 29 */
    0x24400,    /* track 30 */
    0x25600,    /* track 31 */
    0x26700,    /* track 32 */
    0x27800,    /* track 33 */
    0x28900,    /* track 34 */
    0x29a00,    /* track 35 */
    0x2ab00,    /* track 36 */
    0x2bc00,    /* track 37 */
    0x2cd00,    /* track 38 */
    0x2de00,    /* track 39 */
    0x2ef00,    /* track 40 */
};

/* description of a direntry including d64 specific values*/
typedef struct dirent64 {
    /* start position of file */
    unsigned char d_track;
    unsigned char d_sector;
    /* track/sector and offset of where the dirent is stored */
    unsigned char d_detrack;
    unsigned char d_desector;
    unsigned char d_desectpos;
    /* size information */
    unsigned d_blocks;
    unsigned d_size;

    unsigned char d_type;    /* DEL, SEQ, PRG, USR, REL, DIR */
    unsigned char d_locked;    //Bit 6
    unsigned char d_closed;    //Bit 7 produces * if not set
    unsigned char d_ctype;
    char d_petscii_name[MAX_C64_NAME + 1];
} dirent64;

/* dsecription of the surrounding filesys our file is in */
typedef struct d64 {
    FILE* file;

    char header[32];
    char id[6];

    /* d64 specific things */
    unsigned char track;
    unsigned char sector;
    unsigned char track_link;
    unsigned char sector_link;
    unsigned char detrack;
    unsigned char desector;
    unsigned char desectpos;
    unsigned free;
    unsigned sectpos;
    unsigned sectsize;
    unsigned char sectbuf[256];
    unsigned char bam[256];

    /* dirlist things */
    dirent64** dirlist;
    int dirsize;
    int dirpos;
    unsigned char checksum;
    unsigned char supported_tracks;
} d64;
