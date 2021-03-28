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

static BLOCK *allocate(void) {
    if (ghost_root) {
        BLOCK *ptr = ghost_root;
        ghost_root = ptr->chain;
        return ptr;
    }
    if (!dead_array_size) {
        dead_array = (BLOCK *)calloc(QTY_BLOCKS, sizeof(BLOCK));
        if (!dead_array) {
            fprintf(stderr, "Error: Insufficient memory\n");
            exit(1);
        }
        dead_array_size = QTY_BLOCKS;
    }
    return &dead_array[--dead_array_size];
}

static inline void freeblock(BLOCK *ptr) {
    if (ptr->references) ptr->references--;
    else {
        BLOCK *p = ptr;
        for (;;) {
            BLOCK *p2 = p->chain;
            if (!p2) break;
            if (p2->references) {
                p2->references--;
                break;
            }
            p = p2;
        }
        p->chain = ghost_root;
        ghost_root = ptr;
    }
}

static inline int offset_ceiling(int index, int offset_limit) {
    return index > offset_limit ? offset_limit : index < INITIAL_OFFSET ? INITIAL_OFFSET : index;
}

BLOCK* optimize(const unsigned char *input_data, int input_size, int skip, int offset_limit) {
    BLOCK *optimal;
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
        BLOCK last_match;
        int match_length;
    } *arr;
    BLOCK dummy;
    dummy.references = INT_MAX;
    dummy.bits = INT_MAX;

    /* allocate all main data structures at once */
    arr = (struct arr_s *)calloc(max_offset+1, sizeof *arr);
    optimal = (BLOCK *)calloc(input_size+1, sizeof *optimal);
    best_length = (int *)malloc((input_size+1)*sizeof(int));
    if (!arr || !optimal || !best_length) {
         fprintf(stderr, "Error: Insufficient memory\n");
         exit(1);
    }
    best_length[2] = 2;

    /* start with fake block */
    arr[INITIAL_OFFSET].last_match.bits = -1;
    arr[INITIAL_OFFSET].last_match.index = skip-1;
    arr[INITIAL_OFFSET].last_match.offset = INITIAL_OFFSET;
    arr[INITIAL_OFFSET].last_match.length = 0;
    arr[INITIAL_OFFSET].last_match.chain = NULL;

    putchar('[');

    for (offset = 1; offset <= max_offset; offset++) {
        struct arr_s *arr2 = arr + offset;
        arr2->last_literal.index = -INT_MAX;
        arr2->last_literal.chain = &dummy;
    }

    /* process remaining bytes */
    for (index = skip; index < input_size; index++) {
        BLOCK opt, optchain;
        opt.bits = INT_MAX;
        best_length_size = 2;
        max_offset = offset_ceiling(index, offset_limit);
        for (offset = 1; offset <= max_offset; offset++) {
            struct arr_s *arr2 = arr + offset;
            if (index >= offset && input_data[index] == input_data[index - offset] && index != skip) {
                /* copy from last offset */
                if (arr2->last_literal.index != -INT_MAX) {
                    if (arr2->last_literal.index >= 0) {
                        arr2->last_literal.index = ~arr2->last_literal.index;
                        if (arr2->last_match.chain) arr2->last_match.chain->references++;
                        if (arr2->last_literal.chain->references) {
                            arr2->last_literal.chain->references--;
                            arr2->last_literal.chain = allocate();
                        } else {
                            freeblock(arr2->last_literal.chain->chain);
                        }
                        arr2->last_literal.chain->chain = arr2->last_match.chain;
                        arr2->last_literal.chain->bits = arr2->last_match.bits;
                        arr2->last_literal.chain->index = arr2->last_match.index;
                        arr2->last_literal.chain->offset = arr2->last_match.offset;
                        arr2->last_literal.chain->length = arr2->last_match.length;
                        arr2->last_literal.chain->references = 1;
                    } else arr2->last_literal.chain->references++;
                    arr2->last_match.offset = offset;
                    if (arr2->last_match.chain) {
                        if (arr2->last_match.chain->references) {
                            arr2->last_match.chain->references--;
                            arr2->last_match.chain = allocate();
                        } else {
                            freeblock(arr2->last_match.chain->chain);
                        }
                    } else arr2->last_match.chain = allocate();
                    arr2->last_match.chain->chain = arr2->last_literal.chain;
                    arr2->last_match.chain->offset = 0;
                    arr2->last_match.chain->index = ~arr2->last_literal.index;
                    arr2->last_match.chain->length = arr2->last_match.chain->index - arr2->last_literal.chain->index;
                    arr2->last_match.chain->bits = arr2->last_literal.chain->bits + costof_literal(arr2->last_match.chain->length);
                    arr2->last_match.index = index;
                    arr2->last_match.length = index - arr2->last_match.chain->index;
                    arr2->last_match.bits = costof_rep(arr2->last_match.length) + arr2->last_match.chain->bits;
                    if (opt.bits > arr2->last_match.bits) {
                        opt.bits = arr2->last_match.bits;
                        opt.index = index;
                        opt.offset = offset;
                        opt.length = arr2->last_match.length;
                        if (arr2->last_match.chain->chain) arr2->last_match.chain->references++;
                        optchain.chain = arr2->last_match.chain->chain;
                        optchain.bits = arr2->last_match.chain->bits;
                        optchain.index = arr2->last_match.chain->index;
                        optchain.offset = 0;
                        optchain.length = arr2->last_match.chain->length;
                    }
                }
                /* copy from new offset */
                if (++arr2->match_length > 1) {
                    if (best_length_size < arr2->match_length) {
                        int bits = optimal[index - best_length[best_length_size]].bits + costof_run(best_length[best_length_size]-1);
                        do {
                            int bits2 = optimal[index - best_length_size - 1].bits + costof_run(best_length_size);
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
                    int bits = optimal[index - length].bits + costof_match(offset, length);
                    if (arr2->last_match.index != index || arr2->last_match.bits > bits) {
                        if (arr2->last_literal.index >= 0) {
                            arr2->last_literal.index = ~arr2->last_literal.index;
                            if (arr2->last_match.chain) arr2->last_match.chain->references++;
                            if (arr2->last_literal.chain->references) {
                                arr2->last_literal.chain->references--;
                                arr2->last_literal.chain = allocate();
                            } else {
                                freeblock(arr2->last_literal.chain->chain);
                            }
                            arr2->last_literal.chain->chain = arr2->last_match.chain;
                            arr2->last_literal.chain->bits = arr2->last_match.bits;
                            arr2->last_literal.chain->index = arr2->last_match.index;
                            arr2->last_literal.chain->offset = arr2->last_match.offset;
                            arr2->last_literal.chain->length = arr2->last_match.length;
                        }
                        arr2->last_match.bits = bits;
                        arr2->last_match.index = index;
                        arr2->last_match.offset = offset;
                        arr2->last_match.length = length;
                        optimal[index - length].references++;
                        if (arr2->last_match.chain) freeblock(arr2->last_match.chain);
                        arr2->last_match.chain = &optimal[index - length];
                        if (opt.bits > bits) {
                            opt.bits = bits;
                            opt.index = index;
                            opt.offset = offset;
                            opt.length = length;
                            if (arr2->last_match.chain->chain) arr2->last_match.chain->references++;
                            optchain.chain = arr2->last_match.chain->chain;
                            optchain.bits = arr2->last_match.chain->bits;
                            optchain.index = arr2->last_match.chain->index;
                            optchain.offset = arr2->last_match.chain->offset;
                            optchain.length = arr2->last_match.chain->length;
                        }
                    }
                }
            } else {
                /* copy literals */
                arr2->match_length = 0;
                if (arr2->last_match.bits) {
                    arr2->last_literal.index = index;
                    int bits = arr2->last_match.bits + costof_literal(index - arr2->last_match.index);
                    if (opt.bits > bits) {
                        opt.bits = bits;
                        opt.index = index;
                        opt.offset = 0;
                        opt.length = index - arr2->last_match.index;
                        if (arr2->last_match.chain) arr2->last_match.chain->references++;
                        optchain.chain = arr2->last_match.chain;
                        optchain.bits = arr2->last_match.bits;
                        optchain.index = arr2->last_match.index;
                        optchain.offset = arr2->last_match.offset;
                        optchain.length = arr2->last_match.length;
                    }
                }
            }
        }
        if (opt.bits != INT_MAX) {
            optimal[index].bits = opt.bits;
            optimal[index].index = opt.index;
            optimal[index].offset = opt.offset;
            optimal[index].length = opt.length;
            optimal[index].chain = allocate();
            optimal[index].chain->chain = optchain.chain;
            optimal[index].chain->bits = optchain.bits;
            optimal[index].chain->index = optchain.index;
            optimal[index].chain->offset = optchain.offset;
            optimal[index].chain->length = optchain.length;
        }

        if (index*MAX_SCALE/input_size > dots) {
            putchar('.');
            fflush(stdout);
            dots++;
        }
    }

    puts("]");

    return &optimal[input_size-1];
}
