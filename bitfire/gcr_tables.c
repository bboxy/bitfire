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

#include <stdio.h>
const int max_num = 0xf;
const unsigned char result_order[] = "76540213";
const unsigned char gcr[16] = {
	0b00001010,	//0f
	0b00001011,	//07
	0b00010010,	//0d
	0b00010011,	//05
	0b00001110,	//0b
	0b00001111,	//03
	0b00010110,	//09
	0b00010111,	//01
	0b00001001,	//0e
	0b00011001,	//06
	0b00011010,	//0c
	0b00011011,	//04
	0b00001101,	//0a
	0b00011101,	//02
	0b00011110,	//08
	0b00010101	//00
};

//orig mapping
const unsigned char results4hi_orig[16] = {
	0x10,
	0x10,
	0x30,
	0x30,
	0x50,
	0x50,
	0x70,
	0x70,
	0x80,
	0x90,
	0xb0,
	0xb0,
	0xc0,
	0xd0,
	0xf0,
	0xf0
};

const unsigned char results1hi_orig[16] = {
	0x00,
	0x10,
	0x00,
	0x10,
	0x00,
	0x10,
	0x00,
	0x10,
	0x10,
	0x10,
	0x00,
	0x10,
	0x10,
	0x10,
	0x00,
	0x10
};

const unsigned char results5hi[16] = {
	0x00,
	0x10,
	0x20,
	0x30,
	0x40,
	0x50,
	0x60,
	0x70,
	0x80,
	0x90,
	0xa0,
	0xb0,
	0xc0,
	0xd0,
	0xe0,
	0xf0
};

const unsigned char results5lo[16] = {
	0x0,
	0x1,
	0x2,
	0x3,
	0x4,
	0x5,
	0x6,
	0x7,
	0x8,
	0x9,
	0xa,
	0xb,
	0xc,
	0xd,
	0xe,
	0xf
};

const unsigned char results0hi[16] = {
	0x00,
	0x00,
	0x00,
	0x00,
	0x00,
	0x00,
	0x00,
	0x00,
	0x00,
	0x00,
	0x00,
	0x00,
	0x00,
	0x00,
	0x00,
	0x00
};

const unsigned char results0lo[16] = {
	0x0,
	0x0,
	0x0,
	0x0,
	0x0,
	0x0,
	0x0,
	0x0,
	0x0,
	0x0,
	0x0,
	0x0,
	0x0,
	0x0,
	0x0,
	0x0
};

void create_table(const unsigned char* gcr, const unsigned char* res_hi, const unsigned char* res_lo, int* table, char* order, int eor, int mask) {
	int i, j, c;
	unsigned char pos;
	unsigned char res;
	unsigned char bit;
	int encode_lo = 0;
	int encode_hi = 0;
	unsigned char res_;

	for (i = 0; i <= max_num; i++) {
		//XXX TODO overwrites for each run, suck
		for (j = 0; j <= max_num; j++) {
			pos = 0;
			for (c = 0; c < 8; c++) {
				//high
				if (order[c] >= 'A' && order[c] <= 'E') {
					encode_hi = 1;
					bit = order[c] - 'A';
					if (gcr[i] & (1 << (bit))) pos |= (1 << (7 - c));
				}
				//low
				else if (order[c] >= 'a' && order[c] <= 'e') {
					encode_lo = 1;
					bit = order[c] - 'a';
					if (gcr[j] & (1 << (bit))) pos |= (1 << (7 - c));
				}
				//zero
				else {
				}
			}
			res_ = (res_hi[i] | res_lo[j]);
			res = 0;
			for (c = 0; c < 8; c++) {
				res |= ((res_ & (1 << (7 - c))) != 0) << ((result_order[c] - '0'));
			}
			if (encode_lo) res ^= (eor & 0x0f);
			if (encode_hi) res ^= (eor & 0xf0);
			res &= mask;
			table[pos] = res;
		}
	}
}

int merge_table(int* table1, int* table2, int offset, int silent, int merge_lo, int merge_hi) {
	int i;
	for (i = 0; i < 256; i++) {
		//value to check
		if (table2[i] >= 0) {
			if (table1[i + offset] >= 0 && table1[i + offset] != table2[i]) {
				if (merge_lo) {
					if ((table1[i + offset] & 0xf) == 0) {
						table1[i + offset] |= table2[i];
					} else {
						printf("merge collision at %02x with value %02x\n", i, table1[i + offset]);
					}
				} else if (merge_hi) {
					if ((table1[i + offset] & 0xf0) == 0) {
						table1[i + offset] |= table2[i];
					} else {
						printf("merge collision at %02x with value %02x\n", i, table1[i + offset]);
					}
				} else {
					if (silent) return 1;
					printf("collision at %02x with value %02x\n", i, table1[i + offset]);
				}
			} else 	if (table2[i] >= 0) {
				if (i + offset >= 0 && i + offset < 256) {
					table1[i + offset] = table2[i];
				} else {
					if (silent) return 1;
					printf("value out of range $%02x @ $%02x\n", table1[i + offset], i + offset);
				}
			}
		}
	}
	return 0;
}

void print_table(int* table, int from, int to) {
	int i;
	if ((from & 0xf) != 0) {
		printf("\t\t\t!byte ");
		for (i = from & 0xf0; i < from; i++) {
			printf ("     ");
		}
	}
	for (i = from; i <= to; i++) {
		if ((i & 0xf) == 0) {
			printf("\t\t\t!byte ");
		}
		if (table[i] >= 0) printf("$%02x", table[i]);
		else printf ("___");
		if ((i != to) && ((i & 0xf) != 0xf)) printf(", ");
		if ((i & 0xf) == 0xf || i == to) printf("\n");
	}
}

int main () {
	int tabAAAAA000[256];
	int tab0bb00bbb[256];
	int tab00AAAAA0[256];
	int tabbbbbb000[256];
	int tab0000AAAA[256];
	int tab0Abbbbb0[256];
	int tabAAA000AA[256];
	int tab000bbbbb[256];

	int table[256];

	int offset_8 = 0;
	int offset_4 = 0;
	int offset_1 = 0;
	int col;

	int i;

	for (i = 0; i < 256; i++) {
		tabAAAAA000[i] = -1;
		tab0bb00bbb[i] = -1;
		tab00AAAAA0[i] = -1;
		tabbbbbb000[i] = -1;
		tab0000AAAA[i] = -1;
		tab0Abbbbb0[i] = -1;
		tabAAA000AA[i] = -1;
		tab000bbbbb[i] = -1;
		table[i] = -1;
	};

	//           gcr  resulting values hi and low  table         bits       eor   mask
	create_table(gcr, results5hi,      results0lo, tabAAAAA000, "EDCBA...", 0x7f, 0xf0);	//ones
	create_table(gcr, results0hi,      results5lo, tab0bb00bbb, ".ba..edc", 0x7f, 0x0f);	//twos
	create_table(gcr, results5hi,      results0lo, tab00AAAAA0, "..EDCBA.", 0x7f, 0xf0);	//threes
	create_table(gcr, results0hi,      results5lo, tabbbbbb000, "edcba...", 0x7f, 0x0f);	//fours
	create_table(gcr, results4hi_orig, results0lo, tab0000AAAA, "....EDCB", 0x7f, 0xf0);	//fives
	create_table(gcr, results1hi_orig, results5lo, tab0Abbbbb0, ".Aedcba.", 0x7f, 0x1f);	//sixths
	create_table(gcr, results5hi,      results5lo, tabAAA000AA, "CBA...ED", 0x7f, 0xf0);	//sevens
	create_table(gcr, results5hi,      results5lo, tab000bbbbb, "CBAedcba", 0x7f, 0x0f);	//eigths

	//printf("tabAAAAA000\n");
	//print_table(tabAAAAA000, 0, 255);
	//printf("tab0bb00bbb\n");
	//print_table(tab0bb00bbb, 0, 255);
	//printf("tab00AAAAA0\n");
	//print_table(tab00AAAAA0, 0, 255);
	//printf("tabbbbbb000\n");
	//print_table(tabbbbbb000, 0, 255);
	//printf("tab0000AAAA\n");
	//print_table(tab0000AAAA, 0, 255);
	//printf("tab0Abbbbb0\n");
	//print_table(tab0Abbbbb0, 0, 255);
	//printf("tabAAA000AA\n");
	//print_table(tabAAA000AA, 0, 255);
	//printf("tab000bbbbb\n");
	//print_table(tab000bbbbb, 0, 255);

	for (i = 0; i < 256; i++) table[i] = -1;
	merge_table(table, tab0bb00bbb, 0x00, 0, 0, 0);
	printf("tab02200222_lo\n");
	print_table(table, 0, 255);

	for (offset_1 = 0; offset_1 < 256; offset_1++) {
		for (offset_4 = 0; offset_4 < 256; offset_4++) {
			for (offset_8 = 0; offset_8 < 256; offset_8++) {
				for (i = 0; i < 256; i++) table[i] = tabAAA000AA[i];
				col = 0;//merge_table(table, tab0bb00bbb, 0x00, 1, 0, 0);
				if (!col) col = merge_table(table, tab000bbbbb, offset_8, 1, 0, 0);	//88888
				if (!col) col = merge_table(table, tabbbbbb000, offset_4, 1, 0, 0);	//44444
				if (!col) col = merge_table(table, tabAAAAA000, offset_1, 1, 0, 0);	//11111
				if (!col) {
					printf("offset_1: $%02x\n", offset_1);
					printf("offset_4: $%02x\n", offset_4);
					printf("offset_8: $%02x\n", offset_8);
				}
			}
		}
	}

	for (i = 0; i < 256; i++) table[i] = -1;
	merge_table(table, tab000bbbbb, 0x00, 0, 0, 0);	//88888
	merge_table(table, tabAAA000AA, 0x00, 0, 0, 0);	//77777
	//merge_table(table, tab0bb00bbb, 0x00, 0, 0, 0); //22222
	merge_table(table, tabbbbbb000, 0x04, 0, 0, 0);	//44444
	merge_table(table, tabAAAAA000, 0x00, 0, 0, 0);	//11111
	for (i = 0; i < 256; i++) if (table[i] < 0) table[i] = i;
	printf("combined\n");
	print_table(table, 0, 255);
	printf("11111000\n");
	print_table(tabAAAAA000, 0, 255);

	for (i = 0; i < 256; i++) table[i] = -1;
	merge_table(table, tab0000AAAA, 0x00, 0, 0, 0);
	merge_table(table, tab00AAAAA0, 0x00, 0, 0, 0);
	merge_table(table, tab0Abbbbb0, 0x01, 0, 0, 0);
	printf("zeropage\n");
	print_table(table, 0, 255);
	return 0;
}
