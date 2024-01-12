#include <stdio.h>
#define OPERAND		|
#define OPERAND_STR	'|'

int frag4_1 () {
                                            //0101 0
                                            //0101 1
                                            //1001 0
                                            //1001 1
                                            //0111 0
                                            //0111 1
                                            //1011 0
                                            //1011 1
                                            //0100 1
                                            //1100 1
                                            //1101 0
                                            //1101 1
                                            //0110 1
                                            //1110 1
                                            //1111 0
                                            //1010 1

                                            //0101
                                            //0100
                                            //0110
                                            //0111

                                            //1001
                                            //1010
                                            //1011
                                            //1100
                                            //1101
                                            //1110
                                            //1111
    int symr0;
    int symr1;

    int syml0101;
    int syml1001;
    int syml0111;
    int syml1011;
    int syml0100;
    int syml1100;
    int syml1101;
    int syml0110;
    int syml1110;
    int syml1111;
    int syml1010;

    int t;
    int t_max = 0;
    long left;
    long right;
    for (left = 0x01; left < 0x100; left++) {
        symr0 = ((left >> 4) & 0xf);
        symr1 = ((left >> 0) & 0xf);
    printf("loop: %ld\n",left);
    for (right = 0x02400000000; right < 0x100000000000; right++) {

        syml0101 = ((right >> 40) & 0xf);
        syml1001 = ((right >> 36) & 0xf);

        syml0111 = ((right >> 32) & 0xf);
        syml1011 = ((right >> 28) & 0xf);

        syml0100 = ((right >> 24) & 0xf);
        syml1100 = ((right >> 20) & 0xf);

        syml1101 = ((right >> 16) & 0xf);
        syml0110 = ((right >> 12) & 0xf);

        syml1110 = ((right >> 8) & 0xf);
        syml1111 = ((right >> 4) & 0xf);

        syml1010 = ((right >> 0) & 0xf);

        //printf ("%d %d\n", syml0, syml1);

					t = 0;
                                        if ((symr0 OPERAND syml0101) == 0) t++;
                                        if ((symr1 OPERAND syml0101) == 1) t++;
                                        if ((symr0 OPERAND syml1001) == 2) t++;
                                        if ((symr1 OPERAND syml1001) == 3) t++;
                                        if ((symr0 OPERAND syml0111) == 4) t++;
                                        if ((symr1 OPERAND syml0111) == 5) t++;
                                        if ((symr0 OPERAND syml1011) == 6) t++;
                                        if ((symr1 OPERAND syml1011) == 7) t++;

                                        if ((symr1 OPERAND syml0100) == 8) t++;
                                        if ((symr1 OPERAND syml1100) == 9) t++;
                                        if ((symr0 OPERAND syml1101) == 10) t++;
                                        if ((symr1 OPERAND syml1101) == 11) t++;
                                        if ((symr1 OPERAND syml0110) == 12) t++;
                                        if ((symr1 OPERAND syml1110) == 13) t++;
                                        if ((symr0 OPERAND syml1111) == 14) t++;
                                        if ((symr1 OPERAND syml1010) == 15) t++;
                                        if (t > t_max) {
                                            t_max = t;
                                            printf("t_max: %d\n",t_max);
                                            printf("0101 0 -> $%x %c $%x = $%x\n", symr0, OPERAND_STR, syml0101, symr0 OPERAND syml0101);
                                            printf("0101 1 -> $%x %c $%x = $%x\n", symr1, OPERAND_STR, syml0101, symr1 OPERAND syml0101);
                                            printf("1001 0 -> $%x %c $%x = $%x\n", symr0, OPERAND_STR, syml1001, symr0 OPERAND syml1001);
                                            printf("1001 1 -> $%x %c $%x = $%x\n", symr1, OPERAND_STR, syml1001, symr1 OPERAND syml1001);
                                            printf("0111 0 -> $%x %c $%x = $%x\n", symr0, OPERAND_STR, syml0111, symr0 OPERAND syml0111);
                                            printf("0111 1 -> $%x %c $%x = $%x\n", symr1, OPERAND_STR, syml0111, symr1 OPERAND syml0111);
                                            printf("1011 0 -> $%x %c $%x = $%x\n", symr0, OPERAND_STR, syml1011, symr0 OPERAND syml1011);
                                            printf("1011 1 -> $%x %c $%x = $%x\n", symr1, OPERAND_STR, syml1011, symr1 OPERAND syml1011);
                                            printf("0100 1 -> $%x %c $%x = $%x\n", symr1, OPERAND_STR, syml0100, symr1 OPERAND syml0100);
                                            printf("1100 1 -> $%x %c $%x = $%x\n", symr1, OPERAND_STR, syml1100, symr1 OPERAND syml1100);
                                            printf("1101 0 -> $%x %c $%x = $%x\n", symr0, OPERAND_STR, syml1101, symr0 OPERAND syml1101);
                                            printf("1101 1 -> $%x %c $%x = $%x\n", symr1, OPERAND_STR, syml1101, symr1 OPERAND syml1101);
                                            printf("0110 1 -> $%x %c $%x = $%x\n", symr1, OPERAND_STR, syml0110, symr1 OPERAND syml0110);
                                            printf("1110 1 -> $%x %c $%x = $%x\n", symr1, OPERAND_STR, syml1110, symr1 OPERAND syml1110);
                                            printf("1111 0 -> $%x %c $%x = $%x\n", symr0, OPERAND_STR, syml1111, symr0 OPERAND syml1111);
                                            printf("1010 1 -> $%x %c $%x = $%x\n", symr1, OPERAND_STR, syml1010, symr1 OPERAND syml1010);
                                        }
    }
    }
    return 0;
}

int frag1_4 () {
    long s;
    int syml0;
    int syml1;

    int symr0010;
    int symr0011;
    int symr0101;
    int symr0110;
    int symr0111;

    int symr1001;
    int symr1010;
    int symr1011;
    int symr1101;
    int symr1110;
    int symr1111;

    int t;
    int t_max = 0;
    for (s = 0x0200000000000; s < 0x10000000000000; s++) {
        syml0 = ((s >> 48) & 0xf);
        syml1 = ((s >> 44) & 0xf);

        symr0010 = ((s >> 40) & 0xf);
        symr0011 = ((s >> 36) & 0xf);
        symr0101 = ((s >> 32) & 0xf);
        symr0110 = ((s >> 28) & 0xf);
        symr0111 = ((s >> 24) & 0xf);

        symr1001 = ((s >> 20) & 0xf);
        symr1010 = ((s >> 16) & 0xf);
        symr1011 = ((s >> 12) & 0xf);
        symr1101 = ((s >> 8) & 0xf);
        symr1110 = ((s >> 4) & 0xf);
        symr1111 = ((s >> 0) & 0xf);

        //printf ("%d %d\n", syml0, syml1);

					t = 0;
                                        if ((syml0 OPERAND symr1010) == 0) t++;
                                        if ((syml0 OPERAND symr1011) == 1) t++;
                                        if ((syml1 OPERAND symr0010) == 2) t++;
                                        if ((syml1 OPERAND symr0011) == 3) t++;
                                        if ((syml0 OPERAND symr1110) == 4) t++;
                                        if ((syml0 OPERAND symr1111) == 5) t++;
                                        if ((syml1 OPERAND symr0110) == 6) t++;
                                        if ((syml1 OPERAND symr0111) == 7) t++;

                                        if ((syml0 OPERAND symr1001) == 8) t++;
                                        if ((syml1 OPERAND symr1001) == 9) t++;
                                        if ((syml1 OPERAND symr1010) == 10) t++;
                                        if ((syml1 OPERAND symr1011) == 11) t++;
                                        if ((syml0 OPERAND symr1101) == 12) t++;
                                        if ((syml1 OPERAND symr1101) == 13) t++;
                                        if ((syml1 OPERAND symr1110) == 14) t++;
                                        if ((syml1 OPERAND symr0101) == 15) t++;
                                        if (t > t_max) {
                                            t_max = t;
                                            printf("t_max: %d\n",t_max);
                                            printf("0 1010 -> $%x %c $%x = $%x\n", syml0, OPERAND_STR, symr1010, syml0 OPERAND symr1010);
                                            printf("0 1011 -> $%x %c $%x = $%x\n", syml0, OPERAND_STR, symr1011, syml0 OPERAND symr1011);
                                            printf("1 0010 -> $%x %c $%x = $%x\n", syml1, OPERAND_STR, symr0010, syml1 OPERAND symr0010);
                                            printf("1 0011 -> $%x %c $%x = $%x\n", syml1, OPERAND_STR, symr0011, syml1 OPERAND symr0011);
                                            printf("0 1110 -> $%x %c $%x = $%x\n", syml0, OPERAND_STR, symr1110, syml0 OPERAND symr1110);
                                            printf("0 1111 -> $%x %c $%x = $%x\n", syml0, OPERAND_STR, symr1111, syml0 OPERAND symr1111);
                                            printf("1 0110 -> $%x %c $%x = $%x\n", syml1, OPERAND_STR, symr0110, syml1 OPERAND symr0110);
                                            printf("1 0111 -> $%x %c $%x = $%x\n", syml1, OPERAND_STR, symr0111, syml1 OPERAND symr0111);
                                            printf("0 1001 -> $%x %c $%x = $%x\n", syml0, OPERAND_STR, symr1001, syml0 OPERAND symr1001);
                                            printf("1 1001 -> $%x %c $%x = $%x\n", syml1, OPERAND_STR, symr1001, syml1 OPERAND symr1001);
                                            printf("1 1010 -> $%x %c $%x = $%x\n", syml1, OPERAND_STR, symr1010, syml1 OPERAND symr1010);
                                            printf("1 1011 -> $%x %c $%x = $%x\n", syml1, OPERAND_STR, symr1011, syml1 OPERAND symr1011);
                                            printf("0 1101 -> $%x %c $%x = $%x\n", syml0, OPERAND_STR, symr1101, syml0 OPERAND symr1101);
                                            printf("1 1101 -> $%x %c $%x = $%x\n", syml1, OPERAND_STR, symr1101, syml1 OPERAND symr1101);
                                            printf("1 1110 -> $%x %c $%x = $%x\n", syml1, OPERAND_STR, symr1110, syml1 OPERAND symr1110);
                                            printf("1 0101 -> $%x %c $%x = $%x\n", syml1, OPERAND_STR, symr0101, syml1 OPERAND symr0101);
                                        }
                                        if (t == 16) {
                                            printf("ficken\n");
                                        }
    }
    return 0;
}

int frag2_3 () {
    int syml01;
    int syml10;
    int syml11;
    int symr001;
    int symr010;
    int symr011;
    int symr101;
    int symr110;
    int symr111;
    int t;
    int t_max = 0;
    long s;

    for (syml01 = 0; syml01 < 16; syml01++) {
        printf("loop: %d\n",syml01);
    for (s = 0; s < 0x100000000; s++) {
        syml10 = ((s >> 28) & 0xf);
        syml11 = ((s >> 24) & 0xf);

        symr001 = ((s >> 20) & 0xf);
        symr010 = ((s >> 16) & 0xf);
        symr011 = ((s >> 12) & 0xf);
        symr101 = ((s >> 8) & 0xf);
        symr110 = ((s >> 4) & 0xf);
        symr111 = ((s >> 0) & 0xf);

	t = 0;
        if ((syml01 OPERAND symr010) == 0) t++;
        if ((syml01 OPERAND symr011) == 1) t++;
        if ((syml10 OPERAND symr010) == 2) t++;
        if ((syml10 OPERAND symr011) == 3) t++;
        if ((syml01 OPERAND symr110) == 4) t++;
        if ((syml01 OPERAND symr111) == 5) t++;
        if ((syml10 OPERAND symr110) == 6) t++;
        if ((syml10 OPERAND symr111) == 7) t++;

        if ((syml01 OPERAND symr001) == 8) t++;
        if ((syml11 OPERAND symr001) == 9) t++;
        if ((syml11 OPERAND symr010) == 10) t++;
        if ((syml11 OPERAND symr011) == 11) t++;
        if ((syml01 OPERAND symr101) == 12) t++;
        if ((syml11 OPERAND symr101) == 13) t++;
        if ((syml11 OPERAND symr110) == 14) t++;
        if ((syml10 OPERAND symr101) == 15) t++;
        if (t >= t_max) {
            t_max = t;
            printf("t_max: %d\n",t_max);
            printf("01 010 -> $%x %c $%x = $%x\n", syml01, OPERAND_STR, symr010, syml01 OPERAND symr010);
            printf("01 011 -> $%x %c $%x = $%x\n", syml01, OPERAND_STR, symr011, syml01 OPERAND symr011);
            printf("10 010 -> $%x %c $%x = $%x\n", syml10, OPERAND_STR, symr010, syml10 OPERAND symr010);
            printf("10 011 -> $%x %c $%x = $%x\n", syml10, OPERAND_STR, symr011, syml10 OPERAND symr011);
            printf("01 110 -> $%x %c $%x = $%x\n", syml01, OPERAND_STR, symr110, syml01 OPERAND symr110);
            printf("01 111 -> $%x %c $%x = $%x\n", syml01, OPERAND_STR, symr111, syml01 OPERAND symr111);
            printf("10 110 -> $%x %c $%x = $%x\n", syml10, OPERAND_STR, symr110, syml10 OPERAND symr110);
            printf("10 111 -> $%x %c $%x = $%x\n", syml10, OPERAND_STR, symr111, syml10 OPERAND symr111);
            printf("01 001 -> $%x %c $%x = $%x\n", syml01, OPERAND_STR, symr001, syml01 OPERAND symr001);
            printf("11 001 -> $%x %c $%x = $%x\n", syml11, OPERAND_STR, symr001, syml11 OPERAND symr001);
            printf("11 010 -> $%x %c $%x = $%x\n", syml11, OPERAND_STR, symr010, syml11 OPERAND symr010);
            printf("11 011 -> $%x %c $%x = $%x\n", syml11, OPERAND_STR, symr011, syml11 OPERAND symr011);
            printf("01 101 -> $%x %c $%x = $%x\n", syml01, OPERAND_STR, symr101, syml01 OPERAND symr101);
            printf("11 101 -> $%x %c $%x = $%x\n", syml11, OPERAND_STR, symr101, syml11 OPERAND symr101);
            printf("11 110 -> $%x %c $%x = $%x\n", syml11, OPERAND_STR, symr110, syml11 OPERAND symr110);
            printf("10 101 -> $%x %c $%x = $%x\n", syml10, OPERAND_STR, symr101, syml10 OPERAND symr101);
        }
        if (t == 16) {
            printf("ficken\n");
        }
    }
    }
    return 0;
}

int frag3_2 () {
    //010 10
    //100 10
    //011 10
    //101 10
    //110 01
    //111 10

    //010 10
    //010 11
    //010 01

    int symr10;
    int symr11;
    int symr01;
    int syml010;
    int syml100;
    int syml011;
    int syml101;
    int syml110;
    int syml111;
    int t;
    int t_max = 0;
    long s;

    for (symr01 = 0; symr01 < 16; symr01++) {
        printf("loop: %d\n",symr01);
    for (s = 0; s < 0x100000000; s++) {
        symr10 = ((s >> 28) & 0xf);
        symr11 = ((s >> 24) & 0xf);

        syml010 = ((s >> 20) & 0xf);
        syml100 = ((s >> 16) & 0xf);
        syml011 = ((s >> 12) & 0xf);
        syml101 = ((s >> 8) & 0xf);
        syml110 = ((s >> 4) & 0xf);
        syml111 = ((s >> 0) & 0xf);

	t = 0;
        if ((syml010 OPERAND symr10) == 0) t++;
        if ((syml010 OPERAND symr11) == 1) t++;
        if ((syml100 OPERAND symr10) == 2) t++;
        if ((syml100 OPERAND symr11) == 3) t++;
        if ((syml011 OPERAND symr10) == 4) t++;
        if ((syml011 OPERAND symr11) == 5) t++;
        if ((syml101 OPERAND symr10) == 6) t++;
        if ((syml101 OPERAND symr11) == 7) t++;

        if ((syml010 OPERAND symr01) == 8) t++;
        if ((syml110 OPERAND symr01) == 9) t++;
        if ((syml110 OPERAND symr10) == 10) t++;
        if ((syml110 OPERAND symr11) == 11) t++;
        if ((syml011 OPERAND symr01) == 12) t++;
        if ((syml111 OPERAND symr01) == 13) t++;
        if ((syml111 OPERAND symr10) == 14) t++;
        if ((syml101 OPERAND symr01) == 15) t++;
        if (t >= t_max) {
            t_max = t;
            printf("t_max: %d\n",t_max);
            printf("010 10 -> $%x %c $%x = $%x\n", syml010, OPERAND_STR, symr10, syml010 OPERAND symr10);
            printf("010 11 -> $%x %c $%x = $%x\n", syml010, OPERAND_STR, symr11, syml010 OPERAND symr11);
            printf("100 10 -> $%x %c $%x = $%x\n", syml100, OPERAND_STR, symr10, syml100 OPERAND symr10);
            printf("100 11 -> $%x %c $%x = $%x\n", syml100, OPERAND_STR, symr11, syml100 OPERAND symr11);
            printf("011 10 -> $%x %c $%x = $%x\n", syml011, OPERAND_STR, symr10, syml011 OPERAND symr10);
            printf("011 11 -> $%x %c $%x = $%x\n", syml011, OPERAND_STR, symr11, syml011 OPERAND symr11);
            printf("101 10 -> $%x %c $%x = $%x\n", syml101, OPERAND_STR, symr10, syml101 OPERAND symr10);
            printf("101 11 -> $%x %c $%x = $%x\n", syml101, OPERAND_STR, symr11, syml101 OPERAND symr11);
            printf("010 01 -> $%x %c $%x = $%x\n", syml010, OPERAND_STR, symr01, syml010 OPERAND symr01);
            printf("110 01 -> $%x %c $%x = $%x\n", syml110, OPERAND_STR, symr01, syml110 OPERAND symr01);
            printf("110 10 -> $%x %c $%x = $%x\n", syml110, OPERAND_STR, symr10, syml110 OPERAND symr10);
            printf("110 11 -> $%x %c $%x = $%x\n", syml110, OPERAND_STR, symr11, syml110 OPERAND symr11);
            printf("011 01 -> $%x %c $%x = $%x\n", syml011, OPERAND_STR, symr01, syml011 OPERAND symr01);
            printf("111 01 -> $%x %c $%x = $%x\n", syml111, OPERAND_STR, symr01, syml111 OPERAND symr01);
            printf("111 10 -> $%x %c $%x = $%x\n", syml111, OPERAND_STR, symr10, syml111 OPERAND symr10);
            printf("101 01 -> $%x %c $%x = $%x\n", syml101, OPERAND_STR, symr01, syml101 OPERAND symr01);
        }
        if (t == 16) {
            printf("ficken\n");
        }
    }
    }
    return 0;
}

int main () {
    //frag1_4();
    frag3_2();
}
