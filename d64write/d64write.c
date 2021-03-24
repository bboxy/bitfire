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

int debug_level;

#include "includes.h"
#include "d64.h"
#include "debug.h"
#include "debug_funcs.h"

static void ascii2petscii_char(char* c) {
    if(*c>0x60 && *c<=0x7a) *c^=0x20;
    else if (*c>0x40 && *c<=0x5a) *c^=0x80;
}

/*
static void petscii2ascii_char(char* c) {
    if((unsigned char)*c>0xc0 && (unsigned char)*c<=0xda) *c^=0x80;
    else if (*c>0x40 && *c<=0x5a) *c^=0x20;
}
*/

static void ascii2petscii(char* c) {
    while(*c) {
        ascii2petscii_char(c);
        c++;
    }
}

/*
static void petscii2ascii(char* c) {
    while(*c) {
        petscii2ascii_char(c);
        c++;
    }
}
*/

static int d64_set_pos(d64* d64, unsigned char track, unsigned char sector) {
    unsigned int offset;
    /* check vals for track and sector */
    if(track > d64->supported_tracks || sector > sectors[track]) {
        fail_message("illegal track or sector.\n");
        return 0;
    }
    debug_message("track/sector='%d/%d'\n", track, sector);
    /* calculate linear offset in .d64-file */
    offset = track_offsets[track] + (sector * 256);
    debug_message("offset is $%x\n", offset);
    if(fseek(d64->file, offset, SEEK_SET)) {
        fail_message("fssek(): failed\n");
        return 0;
    }
    debug_message("position set successfully\n");
    return 1;
}

static int d64_read_sector(d64* d64, unsigned char track, unsigned char sector, unsigned char* buf) {
    int err;
    debug_message("reading sector %d/%d\n", track, sector);
    /* set position within .d64 */
    if(!d64_set_pos(d64, track, sector)) {
        fail_message("could not set position\n");
        return 0;
    }
    /* read in 256 bytes */
    err = fread(buf, 1, 256, d64->file);

    /* did all go smooth? */
    if(err < 256) {
        fail_message("fread(): could only read %d bytes\n", err);
        perror("\n");
        return 0;
    }
    /* see if sector is partly filled, and calculate sectorsize */
    if(buf[0] == 0) d64->sectsize = buf[1] + 1;
    else d64->sectsize = SECTOR_SIZE;
    debug_message("read %d bytes into buf\n", err);
    return 1;
}

static int d64_write_sector(d64* d64, unsigned char track, unsigned char sector, unsigned char* buf) {
    int err;
    debug_message("writing sector %d/%d\n",track,sector);
    /* set position within .d64 */
    if(!d64_set_pos(d64, track, sector)) {
        fail_message("could not set position\n");
        return 0;
    }
    /* write 256 bytes */
    err = fwrite(buf, 1, 256, d64->file);

    /* did all go smooth? */
    if(err < 256) {
        fail_message("fwrite(): could only write %d bytes\n", err);
        return 0;
    }
    debug_message("wrote %d bytes from buf\n", err);
    return 1;
}

int d64_read_bam(d64* d64) {
    debug_message("reading bam...\n");
    return d64_read_sector(d64, D64_BAM_TRACK, D64_BAM_SECTOR, d64->bam);
}

static int d64_write_bam(d64* d64) {
    debug_message("writing bam...\n");
    return d64_write_sector(d64, D64_BAM_TRACK, D64_BAM_SECTOR, d64->bam);
}

static int d64_get_bam_entry(d64* d64, unsigned char track, unsigned char sector) {
    unsigned char byte;
    int offset;
    /* find position within BAM */
    offset = track * 4;
    /* extend bam dolphin dos style if we wriet more than 36 tracks */
    if (track > 35) offset += 0x1c;
    byte = d64->bam[offset + (sector >> 3) + 1];
    if(byte & ((unsigned char)1) << (sector & 0x7)) return BAM_FREE;
    return BAM_USED;
}

static int d64_set_bam_entry(d64* d64, unsigned char track, unsigned char sector, int used) {
    unsigned char byte;
    int offset;
    int i;
    int free = 0;

    if(used != BAM_USED && used != BAM_FREE) return 0;

    offset = track * 4;
    /* extend bam dolphin dos style if we wriet more than 36 tracks */
    if (track > 35) offset += 0x1c;

    byte = d64->bam[offset + (sector >> 3) + 1];

    byte = byte & (0xff ^ ((unsigned char)1 << (sector & 0x7)));
    byte |= (unsigned char)used << (sector & 0x7);

    d64->bam[offset + (sector >> 3) + 1] = byte;

    /* count free blocks */
    for(i = 0; i < sectors[track]; i++) {
        if(d64_get_bam_entry(d64, track, i) == BAM_FREE) free++;
    }
    /* set new value */
    d64->bam[offset] = free;
    return 1;
}

static int d64_get_free_track_blocks(d64* d64, unsigned char track) {
    int offset;
    if(track < D64_MIN_TRACK || track > d64->supported_tracks) return 0;
    offset = track * 4;
    if (track > 35) offset += 0x1c;
    return d64->bam[offset];
}

void d64_get_free(d64 *d64) {
    int track;
    d64->free = 0;
    for(track = D64_MIN_TRACK; track <= d64->supported_tracks; track ++) {
        if(track != D64_DIR_TRACK) d64->free += d64_get_free_track_blocks(d64, track);
    }
}

/* d64_allocate_next_block
 * allocates next free block on disk, starting from unsigned char track and unsigned char sector
 * on. The new allocated block will be flagged as used in the BAM */

static int d64_allocate_next_block(d64* d64, unsigned char* track, unsigned char* sector, int interleave) {
    int free;
    int revs = 0;
//    int interleaves[] = {3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4};


    debug_message("allocating next block...\n");

    /* choose interleave */
    //if(*track == D64_DIR_TRACK) interleave = DIR_INTERLEAVE;

    //*sector += interleave;
    //if(*sector > sectors[*track]) *sector -= sectors[*track];

    /* let's change track until we have blocks free */
    while(!(free = d64_get_free_track_blocks(d64, *track))) {
        debug_message("no more free blocks on track %d\n", *track);
        /* seems like dirtrack is full, no more space for more entries */
        if(*track == D64_DIR_TRACK) {
            fatal_message("dirtrack full!\n");
            return 0;
        }
        else {
            (*track)++;
            /* don't touch track 18 */
            if(*track == D64_DIR_TRACK) (*track)++;
            /* reach last track? bad luck! */
            if(*track > d64->supported_tracks) {
                fatal_message("disk full!\n");
                return 0;
            }
        }
    }
    debug_message("still '%d' free blocks on track %d\n", free, *track);

//    interleave = interleaves[(*track) - 1];

    /* now as we have a track with free sectors, walk through sector */
    *sector = revs;
    while(d64_get_bam_entry(d64, *track, *sector) == BAM_USED) {
        (*sector) += interleave;
        if(*sector > sectors[*track]) {
            //mext revolution
            revs++;
            *sector = revs;
	}
        if(revs == interleave) {
            /* OOOPS, something really weired happend, bam must be corrupted */
            fatal_message("can't find free block, though should be free (bam corrupted?)\n");
            return 0;
        }
    }
    /* mark found block as used */
    d64_set_bam_entry(d64, *track, *sector, BAM_USED);
    debug_message("next free block is @%d/%d, interleave was %d\n", *track, *sector, interleave);
    return 1;
}

static int d64_display_bam(d64* d64) {
    int free = 0;
    unsigned char track;
    int i;
    int val;
    printf("disk usage (bam)\n");
    printf("---------------sector #------\n");
    printf("       |         111111111122\n");
    printf("track  |123456789012345678901\n");
    printf("-------+---------------------\n");

    for(track = D64_MIN_TRACK; track <= d64->supported_tracks; track++) {
        printf("% 3d    |", track);
        for(i = 0; i < sectors[track]; i++) {
            val = d64_get_bam_entry(d64, track, i);
            if(val == BAM_FREE) {
                printf(" ");
                if (track!= D64_DIR_TRACK) free++;
            }
            else printf(".");
        }
        printf("\n");
    }
    printf("%d blocks free.\n", free);
    return 1;
}

static int d64_update_direntry(d64* d64, char* name, int start_track, int start_sector, int detrack, int desector, int desectpos, int type, int blocks, int locked, int update_link_only) {
    int name_len = 0;
    name_len = strlen(name);

    /* read in sector the direntry is on */
    if(!d64_read_sector(d64, detrack, desector, d64->sectbuf)) return 0;
    /* now write direntry-content  to sectbuf */
    d64->sectbuf[desectpos + D64_DTRACK]  = start_track;
    d64->sectbuf[desectpos + D64_DSECTOR] = start_sector;
    if (!update_link_only) {
        d64->sectbuf[desectpos + D64_DTYPE]   = type;
        d64->sectbuf[desectpos + D64_DTYPE] = d64->sectbuf[desectpos + D64_DTYPE] | 0x80;
        if (locked) d64->sectbuf[desectpos + D64_DTYPE] = d64->sectbuf[desectpos + D64_DTYPE] | 0x40;
        /* add filename */
        if (name_len > 16) {
            fatal_message("name '%s' too long\n", name);
            return 0;
        }
        memcpy(&d64->sectbuf[desectpos + D64_DNAME], name, name_len);
        /* pad filename */
        memset(&d64->sectbuf[desectpos + D64_DNAME + name_len], 0xa0, MAX_C64_NAME - name_len);
        /* add size information */
        d64->sectbuf[desectpos + D64_DBLOCKS + 1] = blocks / 256;
        d64->sectbuf[desectpos + D64_DBLOCKS + 0] = blocks & 0xff;
    }

//    debug_print_hex(&d64->sectbuf[desectpos], 32, "DIR:", 32);

    /* update size info if it is the last dir-sector */
    if(d64->sectbuf[0] == 0) d64->sectbuf[1] = SECTOR_SIZE - 1;

    /* write back to disc */
    if(!d64_write_sector(d64, detrack, desector, d64->sectbuf)) return 0;
    debug_message("updated direntry data: c64_name='%s', startpos='%d/%d', closed='%d', filetype='%s', blocks='%d'\n",
                  name, start_track, start_sector, 0, filetype[type], blocks);

    memset(d64->sectbuf, 0, 256);
    return 1;
}

static int d64_readdir(d64* d64, dirent64* d) {
    int a;
    /* still data to process on current sector? */
    if(d64->sectpos >= d64->sectsize) {
        /* we have a next ts-link? */
        if(d64->track_link) {
            /* follow ts-link */
            d64->track  = d64->track_link;
            d64->sector = d64->sector_link;
            debug_message("trying to read new sector %d/%d\n",d64->track,d64->sector);
            /* get next sector */
            if(!d64_read_sector(d64, d64->track, d64->sector, d64->sectbuf)) return -1;
            /* first, get ts-link from new sector */
            d64->track_link  = d64->sectbuf[0];
            d64->sector_link = d64->sectbuf[1];

            /* ignore size set by read_sector() always walk through whole block, as we have a dir and size does not matter here */
            d64->sectsize = SECTOR_SIZE;
            /* start at pos 0, to have 8 blocks with $20 bytes size, first two bytes (tslink on first entry) will be ignored anyway, but counting is easier */
            d64->sectpos  = 0;
        }
        else {
            debug_message("sector-chain finshed\n");
            return 0;
        }
    }

    debug_message("sectpos='%d' sectsize='%d'\n", (int)d64->sectpos, (int)d64->sectsize);
//    debug_print_hex(&d64->sectbuf[d64->sectpos], 32, "DIR:", 32);

    /* populate direntry with data */
    d->d_desectpos = d64->sectpos;
    d->d_detrack   = d64->track;
    d->d_desector  = d64->sector;
    d->d_type      = d64->sectbuf[d64->sectpos + D64_DTYPE] & 0x7;
    d->d_track     = d64->sectbuf[d64->sectpos + D64_DTRACK];
    d->d_sector    = d64->sectbuf[d64->sectpos + D64_DSECTOR];
    d->d_blocks    = d64->sectbuf[d64->sectpos + D64_DBLOCKS + 1] * 256 + d64->sectbuf[d64->sectpos + D64_DBLOCKS];
    d->d_size      = d->d_blocks * (SECTOR_SIZE - 2);
    if( (d64->sectbuf[d64->sectpos + D64_DTYPE] & 0x80)) d->d_closed = 1;
    else d->d_closed = 0;
    if( (d64->sectbuf[d64->sectpos + D64_DTYPE] & 0x40)) d->d_locked = 1;
    else d->d_locked = 0;
    /* copy filename */
    for(a = 0; a < MAX_C64_NAME; a++) {
        if(d64->sectbuf[d64->sectpos + D64_DNAME + a] == 0xa0) break;
    }
    strncpy((char*)&d->d_petscii_name, (char*)&d64->sectbuf[d64->sectpos + D64_DNAME], a); d->d_petscii_name[a] = 0;

    /* advance to next direntry on sector */
    d64->sectpos += 32;
    return 1;
}

static int d64_create_direntry(d64* d64, char* name, int start_track, int start_sector, int filetype, int blocks, int locked, int link_to, int link_to_num) {
    dirent64 d;
    int status;
    int line = 0;

    /* set startpos */
    d64->track       = D64_DIR_TRACK;
    d64->track_link  = D64_DIR_TRACK;
    d64->sector      = D64_DIR_SECTOR;
    d64->sector_link = D64_DIR_SECTOR;
    /* reset both will force the sector to be read */
    d64->sectpos     = 0;
    d64->sectsize    = 0;

    /* walk through dir to find reusable direntries, position for a found entry will be set appropriately */
    line = 1;
    while((status = d64_readdir(d64, &d))) {
        debug_message("dir-entry found at t/s='%d/%d' @$%02x\n", d.d_detrack, d.d_desector, d.d_desectpos);
        if (line == link_to_num && link_to) break;
        if(!d.d_type && !d.d_closed) break;
        line++;
    }

    if (link_to && line != link_to_num) {
        fatal_message("can't link '%s' to line %d, dir entry does not exist (range is 1 .. %d)!\n", name, link_to_num, line);
    }
    /* nothing free? */
    if(status == 0) {
        debug_message("last block of dir reached, need to extend dir from t/s='%d/%d' on\n", d64->track, d64->sector);
        /* extend dir with new block */
        d64->track_link  = d64->track;
        d64->sector_link = d64->sector;
        d64_allocate_next_block(d64, &d64->track_link, &d64->sector_link, DIR_INTERLEAVE);
        /* update ts-link */
        d64->sectbuf[0] = d64->track_link;
        d64->sectbuf[1] = d64->sector_link;

        /* write sector back to update ts-link */
        d64_write_sector(d64, d64->track, d64->sector, d64->sectbuf);

        /* set position for new direntry */
        d.d_desectpos = 0;
        d.d_detrack   = d64->track_link;
        d.d_desector  = d64->sector_link;
    }

    debug_message("free dir-entry found at t/s='%d/%d' @$%02x\n", d.d_detrack, d.d_desector, d.d_desectpos);

    /* ...and create it on disc */
    //ascii2petscii(name);
    d64_update_direntry(d64, name, start_track, start_sector, d.d_detrack, d.d_desector, d.d_desectpos, filetype, blocks, locked, link_to);
    return 0;
}

/* will change the header/id in d64->bam, bam must still be saved to make changes visible */
static void d64_set_header(d64* d64, char* header, char* id) {
    int i;
    i = strlen(header);
    if(i > MAX_C64_NAME) i = MAX_C64_NAME;
    /* copy new header */
    memcpy(&d64->bam[D64_BAM_HEADER], header, i);
    /* pad remaining space */
    memset(&d64->bam[D64_BAM_HEADER + i], 0xa0, MAX_C64_NAME + 1 - i);
    i = strlen(id);
    if(i > 5) i = 5;
    memcpy(&d64->bam[D64_BAM_ID], id, i);
    memset(&d64->bam[D64_BAM_ID + i], 0x20, 6 - i);
}

int d64_bitfire_set_side(d64* d64, int side) {
    int dirsect = BITFIRE_DIRSECT;
    unsigned char dir[256];
    //first dirsect in use? then read in

    d64_read_sector(d64, D64_DIR_TRACK, dirsect, dir);
    memset(dir, 0, 256);
    dir[0xff] = (side - 1) | 0xf0;
    d64_set_bam_entry(d64, D64_DIR_TRACK, dirsect, BAM_USED);
    d64_write_sector(d64, D64_DIR_TRACK, dirsect, dir);

    return 0;
}

int d64_format(d64* d64, char* header, char* id, int create) {
    int track, sector;
    int k;

    /* start wit ha fresh bam sector */
    memset(d64->bam, 0, 256);

    /* clear bam */
    for(track = D64_MIN_TRACK; track <= d64->supported_tracks; track++) {
        for(sector = 0; sector < sectors[track]; sector++) {
            if (create) {
                for (k = 0; k < 256; k++) fputc(0, d64->file);
            }
            d64_set_bam_entry(d64, track, sector, BAM_FREE);
        }
    }
    /* allocate bam block */
    d64_set_bam_entry(d64, D64_BAM_TRACK, D64_BAM_SECTOR, BAM_USED);
    /* allocate dir block */
    d64_set_bam_entry(d64, D64_DIR_TRACK, D64_DIR_SECTOR, BAM_USED);

    /* set location of dir */
    d64->bam[0] = D64_DIR_TRACK;
    d64->bam[1] = D64_DIR_SECTOR;

    /* set dos-version */
    d64->bam[2] = 0x41;

    /* fill header/id with $a0 */
    memset(d64->bam + 0x90, 0xa0, 0x1a);

    /* now set new header and id */
    d64_set_header(d64, header, id);

    /* write new bam */
    if(!d64_write_bam(d64)) return 0;

    return 1;
}

int d64_create_bitfire_direntry(d64* d64, int track, int sector, int loadaddr, int length, int sectpos, int sectnum, int verbose) {
    int dirsect = BITFIRE_DIRSECT;
    unsigned char dir[256];
    int dir_pos;
    //first dirsect in use? then read in

    while (dirsect > BITFIRE_DIRSECT - 2) {
        if (d64_get_bam_entry(d64, D64_DIR_TRACK, dirsect) == BAM_USED) {
            d64_read_sector(d64, D64_DIR_TRACK, dirsect, dir);
        } else {
            //allocate next block
            memset(dir, 0, 256);
            d64_set_bam_entry(d64, D64_DIR_TRACK, dirsect, BAM_USED);
            //XXX TODO calculate init values from first sector and add them here
        }
        dir_pos = 0;
        while (dir_pos < 63) {
            //empty entries have track set to 0
            if (dir[dir_pos * 4 + 0] + dir[dir_pos * 4 + 1] + dir[dir_pos * 4 + 2] + dir[dir_pos * 4 + 3] == 0) {
                dir[dir_pos * 4 + 0] = loadaddr & 0xff;
                dir[dir_pos * 4 + 1] = (loadaddr >> 8) & 0xff;
                dir[dir_pos * 4 + 2] = (length - 1) & 0xff;
                dir[dir_pos * 4 + 3] = ((length - 1) >> 8) & 0xff;
                //first file in dir sector -> place init values
            	if (dir_pos == 0) {
                    dir[0xfc] = track;
                    dir[0xfd] = sectnum;
                    dir[0xfe] = sectpos;
                    if (verbose) {
                        printf("init-values of dir-sector: track: $%02x, sector-count: $%02x, sector-pos: $%02x\n", track, sectnum, sectpos);
                    }
                }
                d64_write_sector(d64, D64_DIR_TRACK, dirsect, dir);
                return 0;
            }
            dir_pos++;
        }
        dirsect--;
    }
    return 1;
}

void d64_scramble_buffer(unsigned char* buf) {
    int i;
    for (i = 0; i < 256; i += 4) {
        //buf[i + 2] = buf[i + 1] ^ buf [i + 2];
    }
}

int d64_write_file(d64* d64, char* path, int type, int add_dir, int interleave, int verbose, int link_to, int link_to_num) {
    FILE* file;
    int start_track;
    int start_sector;
    int size = 0;
    int data;

    int length = 0;
    char* pname;
    int loadaddr = 0;
    int startpos = d64->sectpos;

    /* at which sector on track do we start? */
    int sectnum = sectors[d64->track] - d64_get_free_track_blocks(d64, d64->track);

    /* partly filled sectors do not count, subtract */
    if (d64->sectpos > 0) sectnum--;

    //printf("track: $%02x used blocks: $%02x\n", d64->track, sectnum);

    if(file = fopen(path, "rb"), !file) {
        fatal_message("unable to open '%s'\n", path);
    }

    d64->checksum = 0;
    /* allocate first free block on disk, thus start with 0/0 */
    if (type != FILETYPE_BITFIRE) {
        if(type == FILETYPE_BOOT) {
            d64->track_link  = D64_DIR_TRACK;
            d64->sector_link = 0;
        } else {
            d64->track_link  = 0;
            d64->sector_link = 0;
        }
        d64_allocate_next_block(d64, &d64->track_link, &d64->sector_link, interleave);
        memset(d64->sectbuf, 0, 256);
    }

    if (type == FILETYPE_BITFIRE) {
        if(d64->sectpos == 0) {
            d64->track_link  = 0;
            d64->sector_link = 0;
            /* start with a new block as last is full or first block */
            d64_allocate_next_block(d64, &d64->track_link, &d64->sector_link, interleave);
            memset(d64->sectbuf, 0, 256);
        } else {
            /* reuse last sector ans start from there on */
            if(!d64_read_sector(d64, d64->track, d64->sector, d64->sectbuf)) return 0;
	    d64->track_link = d64->track;
	    d64->sector_link = d64->sector;
        }
    }

    /* remember startpos */
    start_track = d64->track_link;
    start_sector = d64->sector_link;

    //fetch load address
    loadaddr = fgetc(file);
    loadaddr += fgetc(file) << 8;

    /* init buffer positions */
    if (type == FILETYPE_BITFIRE) {
        //d64->sectpos  = 0;
    } else {
        d64->sectpos  = 2;
        rewind(file);
    }

    d64->sectsize = SECTOR_SIZE;
    /* set startposition */
    d64->track    = d64->track_link;
    d64->sector   = d64->sector_link;

    while((data = fgetc(file)) != EOF) {
        d64->checksum ^= (unsigned char)data;
        length++;
        /* no more space in buffer? */
        if(d64->sectpos == d64->sectsize) {
            /* allocate a new block, therefore set up ts-link */
            d64->track_link = d64->track;
            d64->sector_link = d64->sector;
            d64_allocate_next_block(d64, &d64->track_link, &d64->sector_link, interleave);
            /* count blocks up */
            size++;

            if (type != FILETYPE_BITFIRE) {
                /* set ts-link in finished block */
                d64->sectbuf[0] = d64->track_link;
                d64->sectbuf[1] = d64->sector_link;
            }

            /* write finished sector to disc */
            if(type == FILETYPE_BITFIRE) d64_scramble_buffer(d64->sectbuf);
            if(!d64_write_sector(d64, d64->track, d64->sector, d64->sectbuf)) return 0;

            /* start with an empty block */
            memset(d64->sectbuf, 0, 256);

            /* set up new position */
            d64->track    = d64->track_link;
            d64->sector   = d64->sector_link;
            if (type == FILETYPE_BITFIRE) {
                d64->sectpos  = 0;
            } else {
                d64->sectpos  = 2;
            }
            d64->sectsize = SECTOR_SIZE;
        }

        d64->sectbuf[d64->sectpos] = data;
        d64->sectpos++;
    }

    /* write back bam and dirent with final blocksize in time */
    /* set last ts-link */
    if (type != FILETYPE_BITFIRE) {
        d64->sectbuf[0] = 0x00;
        d64->sectbuf[1] = d64->sectpos - 1;
    }
    /* write last sector */
    if(type == FILETYPE_BITFIRE) d64_scramble_buffer(d64->sectbuf);
    d64_write_sector(d64, d64->track, d64->sector, d64->sectbuf);

    /* blank buffer */
    memset(d64->sectbuf, 0, 256);
//    /* write changed bam */
//    d64_write_bam(d64);

    d64->track_link = start_track;
    d64->sector_link = start_sector;

    /* create direntry */
    if (type != FILETYPE_BITFIRE) {
	if (add_dir) {

#ifdef _MSC_VER
            char drive[_MAX_DRIVE];
            char directory[_MAX_DIR];
            char filename[_MAX_FNAME];
            char extension[_MAX_EXT];
            char pnamebuf[_MAX_PATH];

            _splitpath(path, drive, directory, filename, extension);
            _makepath(pnamebuf, NULL, NULL, filename, extension);
            pname=pnamebuf;
#else
            pname = basename(path);
#endif

            ascii2petscii(pname);
            d64_create_direntry(d64, pname, start_track, start_sector, FILETYPE_PRG, size, 0, link_to, link_to_num);
        }
    } else {
        if (d64_create_bitfire_direntry(d64, start_track, d64->sector_link, loadaddr, length, startpos, sectnum, verbose) != 0) {
            fatal_message("Error adding dirent for '%s'. Dir full?\n", path);
        }
    }
    if (verbose) {
        switch (type) {
            case FILETYPE_BOOT:
                printf("type: bootfile  mem: $%04x-$%04x  size:% 4d block%s ($%04x) starting @ %02d/%02d  checksum: $%02x  path: \"%s\"\n", loadaddr, loadaddr + length, (length / 254) + 1, ((length / 254) + 1) > 1 ? "s":" ", length, start_track, start_sector, d64->checksum, path);
            break;
            case FILETYPE_STANDARD:
                printf("type: standard  mem: $%04x-$%04x  size:% 4d block%s ($%04x) starting @ %02d/%02d  checksum: $%02x  path: \"%s\"", loadaddr, loadaddr + length, (length / 254) + 1, ((length / 254) + 1) > 1 ? "s":" ", length, start_track, start_sector, d64->checksum, path);
                if (link_to) printf("  line-link: %d", link_to_num);
                printf("\n");
            break;
            case FILETYPE_BITFIRE:
                printf("type: bitfire   mem: $%04x-$%04x  size:% 4d block%s ($%04x) starting @ %02d/%02d  checksum: $%02x  last_sect_size: $%03lx  path: \"%s\"\n", loadaddr, loadaddr + length, (length / 256) + 1, ((length / 256) + 1) > 1 ? "s":" ", length, start_track, start_sector, d64->checksum, d64->sectpos, path);
            break;
        }
    }
    return 0;
}

static unsigned char s2p[] = {
    0x40,0x41,0x42,0x43,0x44,0x45,0x46,0x47,0x48,0x49,0x4a,0x4b,0x4c,0x4d,0x4e,0x4f,
    0x50,0x51,0x52,0x53,0x54,0x55,0x56,0x57,0x58,0x59,0x5a,0x5b,0x5c,0x5d,0x5e,0x5f,
    0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,0x28,0x29,0x2a,0x2b,0x2c,0x2d,0x2e,0x2f,
    0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x3a,0x3b,0x3c,0x3d,0x3e,0x3f,
    0xc0,0xc1,0xc2,0xc3,0xc4,0xc5,0xc6,0xc7,0xc8,0xc9,0xca,0xcb,0xcc,0xcd,0xce,0xcf,
    0xd0,0xd1,0xd2,0xd3,0xd4,0xd5,0xd6,0xd7,0xd8,0xd9,0xda,0xdb,0xdc,0xdd,0xde,0xdf,
    0x20,0xa1,0xa2,0xa3,0xa4,0xa5,0xa6,0xa7,0xa8,0xa9,0xaa,0xab,0xac,0xad,0xae,0xaf,
    0xb0,0xb1,0xb2,0xb3,0xb4,0xb5,0xb6,0xb7,0xb8,0xb9,0xba,0xbb,0xbc,0xbd,0xbe,0xbf,
    0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0a,0x0b,0x0c,0x0d,0x0e,0x0f,
    0x10,0x11,0x12,0x20,0x20,0x15,0x16,0x17,0x18,0x19,0x1a,0x1b,0x1c,0x1d,0x1e,0x1f,
    0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,
    0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,
    0x80,0x81,0x82,0x83,0x84,0x85,0x86,0x87,0x88,0x89,0x8a,0x8b,0x8c,0x20,0x8e,0x8f,
    0x90,0x91,0x92,0x93,0x94,0x95,0x96,0x97,0x98,0x99,0x9a,0x9b,0x9c,0x9d,0x9e,0x9f,
    0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,
    0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20
};

void screen2petscii(char* data, int size) {
    int i;
    for(i = 0; i < size; i++) {
        data[i] = s2p[(unsigned char)data[i]];
    }
}

//XXX TODO locked und Filetype
void d64_apply_dirart(d64* d64, char* art_path, int boot_track, int boot_sector, int lines) {
    char art[41] = { 0 };
    char header[17] = { 0 };
    char id[6] = { 0 };
    char filename[17] = { 0 };
    int c, i, j;
    FILE* file;
    int head;
    int filetype;
    int locked;
    int blocks;

    if(file = fopen(art_path, "rb+"), !file) {
        fatal_message("unable to open '%s'\n", art_path);
    }
    c = fgetc(file);
    c = fgetc(file);
    j = 0;
    head = 0;
    while((c = fgetc(file)) != EOF && lines) {
        if (j < 39) art[j++] = c;
        else {
            art[j] = 0;
            if (head == 0) {
                for (i = 0; i < 39; i++) art[i] = art[i] & 0x7f;
                memcpy(header, art + 3, 16);
                memcpy(id, art + 21, 5);
                screen2petscii(header, 16);
                screen2petscii(id, 5);
                d64_set_header(d64, header, id);
                head++;
            } else {
                memcpy(filename, art + 6, 16);
                screen2petscii(filename, 16);
                blocks = strtoul(art, NULL, 10);
                locked = art[27] == 0x3c;
                switch (art[24]) {
                    case 0x13:
                        filetype = FILETYPE_SEQ;
                    break;
                    case 0x10:
                        filetype = FILETYPE_PRG;
                    break;
                    case 0x04:
                        filetype = FILETYPE_DEL;
                    break;
                    case 0x12:
                        filetype = FILETYPE_REL;
                    break;
                    case 0x15:
                        filetype = FILETYPE_USR;
                    break;
                    default:
                        filetype = FILETYPE_DEL;
                    break;
                }
                d64_create_direntry(d64, filename, boot_track, boot_sector, filetype, blocks, locked, 0, 0);
                lines--;
            }
            j = 0;
        }
    }
    fclose(file);
    return;
}

int main(int argc, char *argv[]) {
    d64 d64;
    int side = -1;

    int c;

    char* filename = NULL;
    char* d64_path = NULL;
    char* art_path = NULL;
    char* boot_file = NULL;

    char d64_default_header[] = "none";
    char d64_default_id[] = "00";

    char* d64_header = d64_default_header;
    char* d64_id = d64_default_id;

    int lines = 0;
    int dir_art = 0;
    int interleave = FILE_INTERLEAVE;
    int format = 0;
    int verbose = 0;
    int link_to = 0;

    int link_to_num = 0;

    debug_level = 0;
    d64.supported_tracks = 35;

    c = 0;
    if (argc <= 1) {
        printf("Usage: d64write (-c|-d) diskimage.d64 -h header -i id -s standard_format.prg -b bitfire_format.prg --boot bootloader.prg --side 1 -a 12 dirart.prg --interleave 5\n");
        printf("A multiple of files can be given as argument using -s or -b multiple times.\n");
        printf("-c <d64-image>          Select imgae to write to and create/format it.\n");
        printf("-d <d64-image>          Select image. Files will be added.\n");
        printf("-b <file>               Writes a file in bitfire format without a visible dir entry.\n");
        printf("-s <file> [line]        Writes a file in standard format. Optionally it will linked to the line of dir-art if given.\n");
        printf("--side <num>            Determines which side this disk image will be when it comes about turning the disc.\n");
        printf("--boot <file>           Writes a standard file into the dirtrack. The dirart is linked to that file.\n");
        printf("-a <num> <dirart.prg>   A dirart can be provided, it extracts the first 16 chars of <num> lines of a petscii screen plus a first line that is interpreted as header + id. Any header and id given through -h and -i will be ignored then.\n");
        printf("--interleave <num>      Write files with given interleave (change that value also in config.inc). Default: %d\n", interleave);
        printf("--40                    Enable 40 track support.\n");
        printf("-v			Verbose output.\n");
        exit (0);
    }

    //parse out and check options
    while(++c < argc) {
        if(!strcmp(argv[c], "-h")) {
            if (argc -c > 1) d64_header = argv[++c];
            else {
                fatal_message("missing value for option '%s'\n", argv[c]);
            }
        }
        else if(!strcmp(argv[c], "-i")) {
            if (argc - c > 1) d64_id = argv[++c];
            else {
                fatal_message("missing value for option '%s'\n", argv[c]);
            }
        }
        else if(!strcmp(argv[c], "--interleave")) {
            if (argc - c > 1) interleave = strtoul(argv[++c], NULL, 10);
            else {
                fatal_message("missing value for option '%s'\n", argv[c]);
            }
            if (interleave < 1 || interleave > 16) {
                fatal_message("value for interleave must be in the range from 1 to 16\n");
            }
        }
        else if(!strcmp(argv[c], "--side")) {
            if (argc -c > 1) side = strtoul(argv[++c], NULL, 10);
            else {
                fatal_message("missing value for option '%s'\n", argv[c]);
            }
            if (side < 1 || side > 16) {
                fatal_message("value for side must be in the range from 1 to 16\n");
            }
        }
        else if(!strcmp(argv[c], "-c")) {
            if (d64_path) {
                fatal_message("image name already given ('%s')\n", d64_path);
            }
            if (argc -c > 1) d64_path = argv[++c];
            else {
                fatal_message("missing path for option '%s'\n", argv[c]);
            }
            format = 1;
        }
        else if(!strcmp(argv[c], "-d")) {
            if (d64_path) {
                fatal_message("image name already given ('%s')\n", d64_path);
            }
            if (argc -c > 1) d64_path = argv[++c];
            else {
                fatal_message("missing path for option '%s'\n", argv[c]);
            }
        }
        else if(!strcmp(argv[c], "-s")) {
            c++;
            if (argc -c > 1) {
                link_to_num = strtoul(argv[++c], NULL, 10);
		if (!errno) c++;
            }
        }
        else if(!strcmp(argv[c], "-b")) {
            c++;
        }
        else if(!strcmp(argv[c], "--boot")) {
            if (argc -c > 1) boot_file = argv[++c];
            else {
                fatal_message("missing path for option '%s'\n", argv[c]);
            }
        }
        else if(!strcmp(argv[c], "--40")) {
            d64.supported_tracks = 40;
        }
        else if(!strcmp(argv[c], "-v")) {
            verbose = 1;
        }
        else if(!strcmp(argv[c], "-a")) {
            if (argc -c > 1) lines = strtoul(argv[++c], NULL, 10);
            else {
                fatal_message("missing value for option '%s'\n", argv[c]);
            }
            if (argc -c > 1) art_path = argv[++c];
            else {
                fatal_message("missing path for option '%s'\n", argv[c-1]);
            }
            dir_art = 1;
        }
        else {
            fatal_message("unknown option '%s'\n", argv[c]);
        }
    }

    //all sane so far

    //parse out diskimage and open it now as we have possible params like header and id
    if (format) {
        if(d64.file = fopen(d64_path, "wb"), !d64.file) {
            fatal_message("unable to open '%s'\n", d64_path);
        }

        ascii2petscii(d64_header);
        ascii2petscii(d64_id);
        //creates empty file
        d64_format(&d64,d64_header,d64_id,1);
        fclose(d64.file);
    }

    //now open d64
    if(d64.file = fopen(d64_path, "rb+"), !d64.file) {
        fatal_message("unable to open '%s'\n", d64_path);
    }

    d64_read_bam(&d64);
    if (side > 0) d64_bitfire_set_side(&d64, side);

    //parse out bitfire files and write them to d64
    c = 0;
    d64.sectpos = 0;
    while(++c < argc) {
        if(argc -c > 1 && !strcmp(argv[c], "-b")) {
            if (!format) {
                fatal_message("bitfire files will only be written to a fresh disc, to avoid loss of standard files, use the -c option!\n");
            }
            d64_write_file(&d64, argv[++c], FILETYPE_BITFIRE, 1, interleave, verbose, 0, 0);
        }
    }

    //write the boot file
    if(boot_file) {
        d64_write_file(&d64, boot_file, FILETYPE_BOOT, dir_art ^ 1, DIR_INTERLEAVE, verbose, 0, 0);
    }

    //and a dir art linked to that now as we have track/sector info for the bootfile
    if(dir_art) d64_apply_dirart(&d64, art_path, d64.track_link, d64.sector_link, lines);

    //finally add the standard files
    c = 0;
    while(++c < argc) {
        if(argc -c > 1 && !strcmp(argv[c], "-s")) {
            filename = argv[++c];
            if (argc -c > 1) {
                link_to_num = strtoul(argv[++c], NULL, 10);
                if (errno != 0) link_to = 0;
                else link_to = 1;
            } else {
                link_to = 0;
            }
//            if (line < 1 && !errno) {
//                fatal_message("can't link file '%s' to line nummer %d\n", filename, line);
//            }
            if (!dir_art) {
                printf("ignoring linenumber for standard-file '%s', as no dir-art ist used.\n", filename);
                link_to = 0;
            }
            d64_write_file(&d64, filename, FILETYPE_STANDARD, 1, interleave, verbose, link_to, link_to_num);
        }
    }

    if (verbose) d64_display_bam(&d64);
    d64_write_bam(&d64);

    fclose(d64.file);
    exit(0);
}

