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

#include "compress.h"
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#include "zx0.h"

static unsigned char* output_data;
static int output_index;
static int input_index;
static int bit_index;
static int bit_mask;
static int diff;
//static int backtrack;

static void read_bytes(int n, int *delta) {
    input_index += n;
    diff += n;
    if (diff > *delta)
        *delta = diff;
}

static void write_byte(int value) {
    output_data[output_index++] = value;
    diff--;
}

static void write_bit(int value) {
//    if (backtrack) {
//        if (value)
//            output_data[output_index-1] |= 1;
//        backtrack = FALSE;
//    } else {
        if (!bit_mask) {
            bit_mask = 128;
            bit_index = output_index;
            write_byte(0);
        }
        if (value)
            output_data[bit_index] |= bit_mask;
        bit_mask >>= 1;
//    }
}

static void write_interlaced_elias_gamma(int value, int backwards_mode, int skip, int invert) {
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
	if (count >= 8) invert = 0;
        count++;
        if (!skip) write_bit(0);
        //if (!skip) write_bit(backwards_mode);
	skip = 0;
        if (!skip) write_bit(((value & i) > 0) ^ invert);
    }
    if (!skip) write_bit(1);
    //if (!skip) write_bit(!backwards_mode);
}

static void encode_literal(int length, const unsigned char *input_data, int *delta, int backwards_mode, int first) {
    int i;
    if (!first) write_bit(0);

    /* copy literals length */
    write_interlaced_elias_gamma(length, backwards_mode, 0, 0);
    /* copy literals values */
    for (i = 0; i < length; i++) {
        write_byte(input_data[input_index]);
        read_bytes(1, delta);
    }
    return;
}

static void encode_rep(int length, int *delta, int backwards_mode) {
    write_bit(0);
    /* copy from last offset length */
    write_interlaced_elias_gamma(length, backwards_mode, 0, 0);
    read_bytes(length, delta);
    return;
}

static void encode_match(int length, int offset, int *delta, int backwards_mode) {
    write_bit(1);
    /* copy from new offset MSB, -1 to make use of full offset range + 1 as 0 is not allowed in elias gamma */
    write_interlaced_elias_gamma((offset - 1) / 128 + 1, backwards_mode, 0, 0);
    write_byte((((offset - 1) % 128) << 1) | (length == 2));

     /* copy from new offset length */
    write_interlaced_elias_gamma(length-1, backwards_mode, 1, 0);
    read_bytes(length, delta);
    return;
}

unsigned char *compress(BLOCK *optimal, const unsigned char *input_data, int input_size, int skip, int backwards_mode, int *output_size, int *delta, int inplace, int *inplace_end_pos) {
    BLOCK *next;
    BLOCK *prev;
    int last_offset = INITIAL_OFFSET;
    int first = TRUE;
    int i;
    int remaining;
    int inplace_output_index = 0;
    int inplace_input_index = 0;
    int overwrite;
    //int expected = 0;
    int actual = 0;

    /* calculate and allocate output buffer */
    if (!inplace) {
       /* add end-marker */
       *output_size = (optimal->bits + 18 + 7);
    } else {
       *output_size = (optimal->bits + 7);
    }
    //expected = *output_size;
    *output_size >>= 3;
    output_data = (unsigned char *)malloc(*output_size);
    if (!output_data) {
         fprintf(stderr, "Error: Insufficient memory\n");
         exit(1);
    }

    /* initialize delta */
    diff = *output_size - input_size + skip;
    *delta = 0;

    /* un-reverse optimal sequence */
    next = NULL;
    while (optimal) {
        prev = optimal->chain;
        optimal->chain = next;
        next = optimal;
        optimal = prev;
    }

    input_index = skip;
    output_index = 0;
    bit_mask = 0;

    inplace_output_index = output_index;
    inplace_input_index = input_index;

    //printf("expected: %d\n", expected / 8);

    for (optimal = next->chain; optimal; optimal = optimal->chain) {
        if (!optimal->offset) {
            //printf("literal: len $%04x\n", optimal->length);
            encode_literal(optimal->length, input_data, delta, backwards_mode, first);
            actual += costof_literal(optimal->length);
            /* copy literals indicator */
            if (first) first = FALSE;
        } else if (optimal->offset == last_offset) {
            //printf("copy last offset: $%04x len $%04x\n", optimal->offset, optimal->length);
            /* copy from last offset indicator */
            encode_rep(optimal->length, delta, backwards_mode);
            actual += costof_rep(optimal->length);
        } else {
            /* copy from new offset indicator */
            //printf("copy new offset: $%04x len $%04x\n", optimal->offset, optimal->length);
            encode_match(optimal->length, optimal->offset, delta, backwards_mode);
            last_offset = optimal->offset;
            actual += costof_match(optimal->offset, optimal->length);
        }

        if (inplace) {
            /* added +1, was off by one, thanks to Krill! */
            overwrite = (input_index) - (input_size - *output_size + output_index) + 1;
            /* we would overwrite our packed data with a match, or have a literal that directly is followed by plain literal, so they can be aggregated */
            /* it is a literal that woudl overwrite, so exit already here */
            if (overwrite >= 0 && !optimal->offset) break;
            /* accept match and update end_position, literals are skipped then, as they fall back to last position */
            *inplace_end_pos = input_index - skip;
            inplace_output_index = output_index;
            inplace_input_index = input_index;
            /* it is a match that overwrites, let it still happen, but then end with plain literal */
            if (overwrite >= 0 && optimal->offset) break;
        }
    }

    //printf("actual: %d\n", expected);

    /* no endmarker in case of inplace depacking */
    if (!inplace) {
        *inplace_end_pos = input_index - skip;
        /* end marker */
        write_bit(1);
        write_interlaced_elias_gamma(256, backwards_mode, 0, 0);
    } else {
        /* copy remaining data as is */
        input_index = inplace_input_index;
        output_index = inplace_output_index;
        remaining = input_size - inplace_input_index;
	*output_size = inplace_output_index + remaining;
        for (i = 0; i < remaining; i++) {
            write_byte(input_data[input_index]);
            read_bytes(1, delta);
        }
    }

    return output_data;
}
