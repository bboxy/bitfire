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

#include "optimize.h"
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#include "zx0.h"

#define MAX_SCALE 50

#define QTY_BLOCKS 10000

static BLOCK *ghost_root = NULL;
static BLOCK *dead_array = NULL;
static int dead_array_size = 0;

static BLOCK *allocate(int bits, int index, int offset, int length, BLOCK *chain) {
    BLOCK *ptr;

    if (ghost_root) {
        ptr = ghost_root;
        ghost_root = ptr->chain;
    } else {
        if (!dead_array_size) {
            dead_array = (BLOCK *)calloc(QTY_BLOCKS, sizeof(BLOCK));
            if (!dead_array) {
                fprintf(stderr, "Error: Insufficient memory\n");
                exit(1);
            }
            dead_array_size = QTY_BLOCKS;
        }
        ptr = &dead_array[--dead_array_size];
    }
    ptr->bits = bits;
    ptr->index = index;
    ptr->offset = offset;
    ptr->length = length;
    ptr->chain = chain;
    return ptr;
}

static inline void freeblock(BLOCK *ptr) {
    if (ptr->references) ptr->references--;
    else {
        if (ptr->chain) freeblock(ptr->chain);
        ptr->chain = ghost_root;
        ghost_root = ptr;
    }
}

static inline int offset_ceiling(int index, int offset_limit) {
    return index > offset_limit ? offset_limit : index < INITIAL_OFFSET ? INITIAL_OFFSET : index;
}

//XXX TODO build in cost model, so splittet matches get also their costs
//functions for encode_literal/rep/match
BLOCK* optimize(const unsigned char *input_data, int input_size, int skip, int offset_limit) {
    BLOCK **optimal;
    int* best_length;
    int best_length_size;
    int index;
    int offset;
    int dots = 2;
    int max_offset = offset_ceiling(input_size-1, offset_limit);
    struct arr_s {
        struct {
            BLOCK *chain;
            int index;
        } last_literal;
        BLOCK *last_match;
        int match_length;
    } *arr;
    BLOCK dummy;
    dummy.references = INT_MAX;
    dummy.bits = INT_MAX;

    /* allocate all main data structures at once */
    arr = (struct arr_s *)calloc(max_offset+1, sizeof *arr);
    optimal = (BLOCK **)calloc(input_size+1, sizeof(BLOCK *));
    best_length = (int *)malloc((input_size+1)*sizeof(int));
    if (!arr || !optimal || !best_length) {
         fprintf(stderr, "Error: Insufficient memory\n");
         exit(1);
    }
    best_length[2] = 2;

    /* start with fake block */
    arr[INITIAL_OFFSET].last_match = allocate(-1, skip-1, INITIAL_OFFSET, 0, NULL);

    printf("[");

    /* process remaining bytes */
    for (index = skip; index < input_size; index++) {
        BLOCK *opt = &dummy;
        best_length_size = 2;
        max_offset = offset_ceiling(index, offset_limit);
        for (offset = 1; offset <= max_offset; offset++) {
            struct arr_s *arr2 = arr + offset;
            if (index >= offset && input_data[index] == input_data[index - offset] && index != skip) {
                /* copy from last offset */
                if (arr2->last_literal.chain) {
                    int length2 = arr2->last_literal.index - arr2->last_literal.chain->index;
                    int bits2 = arr2->last_literal.chain->bits + costof_literal(length2);
                    int length = index - arr2->last_literal.index;
                    int bits = costof_rep(length) + bits2;
                    arr2->last_literal.chain->references++;
                    if (arr2->last_match) freeblock(arr2->last_match);
                    arr2->last_match = allocate(bits, index, offset, length, allocate(bits2, arr2->last_literal.index, 0, length2, arr2->last_literal.chain));
                    if (opt->bits > bits) {
                        arr2->last_match->references++;
                        freeblock(opt);
                        opt = arr2->last_match;
                    }
                }
                /* copy from new offset */
                if (++arr2->match_length > 1) {
                    if (best_length_size < arr2->match_length) {
                        int bits = optimal[index - best_length[best_length_size]]->bits + costof_run(best_length[best_length_size]-1);
                        do {
                            int bits2 = optimal[index-best_length_size-1]->bits + costof_run(best_length_size);
                            best_length_size++;
                            if (bits2 <= bits) {
                                best_length[best_length_size] = best_length_size;
                                bits = bits2;
                            } else {
                                best_length[best_length_size] = best_length[best_length_size-1];
                            }
                        } while (best_length_size < arr2->match_length);
                    }
                    int length = best_length[arr2->match_length];
                    int bits = optimal[index - length]->bits + costof_match(offset, length);
                    if (!arr2->last_match || arr2->last_match->index != index || arr2->last_match->bits > bits) {
                        optimal[index - length]->references++;
                        if (arr2->last_match) freeblock(arr2->last_match);
                        arr2->last_match = allocate(bits, index, offset, length, optimal[index - length]);
                        if (opt->bits > bits) {
                            arr2->last_match->references++;
                            freeblock(opt);
                            opt = arr2->last_match;
                        }
                    }
                }
            } else {
                /* copy literals */
                arr2->match_length = 0;
                if (arr2->last_match) {
                    if (arr2->last_literal.chain != arr2->last_match) {
                        //if (arr2->last_literal.chain) freeblock(arr2->last_literal.chain);
                        arr2->last_match->references++;
                        arr2->last_literal.chain = arr2->last_match;
                    }
                    arr2->last_literal.index = index;
                    int bits = arr2->last_match->bits + costof_literal(index - arr2->last_match->index);
                    if (opt->bits > bits) {
                        arr2->last_match->references++;
                        freeblock(opt);
                        opt = allocate(bits, index, 0, index - arr2->last_match->index, arr2->last_match);
                    }
                }
            }
        }
        optimal[index] = opt;

        if (index*MAX_SCALE/input_size > dots) {
            printf(".");
            fflush(stdout);
            dots++;
        }
    }

    printf("]\n");

    return optimal[input_size-1];
}
