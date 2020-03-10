#include <stdint.h>
#include <stdlib.h>
#include "mt19937ar/mt19937ar.h"

typedef int bool;
#define true 1
#define false 0

#define swap(t, x, y) {register t tmp = y; y = x; x = tmp;}

#define ALTO  16
#define ANCHO 64
#define Q_ALTO  0XF
#define Q_ANCHO 0X3F


typedef struct {
    uint8_t x;
    uint8_t y;
} pointxy;

// from uint32_t to pointxy format
#define extract_ancho(n) ( (int) (n & Q_ANCHO) )
#define extract_alto(n) ( (int) ((n>>16) & Q_ALTO) )

#define START {.x = 0, .y = 0}
#define END   {.x = ANCHO-1, .y = ALTO-1}

// ========== structure casilla ==========  
//  1 1 1 1 1 1 1 
//  6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
// +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
// | | | | | | | | | |l|d|r|u| | | |m| 
// +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
// ^                 ^       ^     ^
// |-----------------|       |-----|
//      usado por el algoritmo
//   para construir el laberinto
//
// m = marked
// ldru = walls (left, down, right, up)
typedef uint16_t casilla;

#define mark(m, p) (m[p.y][p.x] |= 0x1)
#define is_marked(c) (c & 0x1)

// walls
//    1       u       0
//  8 x 2   l · r   2 n 1
//    4       d       3
#define create_walls1(n) (n |= 0xF0)
#define open_left1(n)  (n &= (~0x0)^0x80)
#define open_right1(n) (n &= (~0x0)^0x20)
#define open_up1(n)    (n &= (~0x0)^0x10)
#define open_down1(n)  (n &= (~0x0)^0x40)

#define create_walls(m, p) (m[p.y][p.x] |= 0xF0)
#define open_left(m, p)  (m[p.y][p.x] &= (~0x0)^0x80)
#define open_right(m, p) (m[p.y][p.x] &= (~0x0)^0x20)
#define open_up(m, p)    (m[p.y][p.x] &= (~0x0)^0x10)
#define open_down(m, p)  (m[p.y][p.x] &= (~0x0)^0x40)

#define is_open_left(n)  ( !(n & 0x80) )
#define is_open_right(n) ( !(n & 0x20) )
#define is_open_up(n)    ( !(n & 0x10) )
#define is_open_down(n)  ( !(n & 0x40) )

#define UP    0
#define RIGHT 1
#define DOWN  3
#define LEFT  2
#define random_move() ( (int) genrand_int32() & 0x3 )
#define inverse_mov(m) ( (~m) & 0x3 )


// open walls: if UD then RL closed
//    1
//  8 x 2
//    4
#define UD 0xA
#define UR 0xC
#define UL 0x6
#define LD 0x3
#define RD 0x9
#define RL 0x5
void printMazePaths(casilla **matrix, int ancho, int alto) {
    for(int i=0; i<alto; i++) {
        for(int j=0; j<ancho; j++) {
            if( is_marked(matrix[i][j]) )
                switch ((matrix[i][j] >> 4) & 0xF) {
                    case UD: printf("│"); break;
                    case UR: printf("└"); break;
                    case UL: printf("┘"); break;
                    case LD: printf("┐"); break;
                    case RD: printf("┌"); break;
                    case RL: printf("─"); break;
                    default: printf("x");
                }
            else printf(" ");
        }
        printf("%X \n", i);
    }
}

/* qsort int comparison function */
// int int_cmp(const void *a, const void *b)
// {
//     const int *ia = (const int *)a; // casting pointer types
//     const int *ib = (const int *)b;
//     return *ia  - *ib;
// 	/* integer comparison: returns negative if b > a
// 	and positive if a > b */
// }

// int quoting_num(int n) {
//     if(n>0x10000) exit(1); // descartar si es mayor a 2^16
//     n-=1;
//     int i;
//     for(i=0; n!=0; i=(i<<1)+1)
//         n>>=1;
//     return i;
// }

// void printA(char a[], int size)
// {
//    printf("[");
// 
//    int i;
//    for(i=0; i<size; i++){
//       printf("%d", a[i]);
//       if(i==size-1) break;
//       printf(", ");
//    }
// 
//    printf("]");
// }
