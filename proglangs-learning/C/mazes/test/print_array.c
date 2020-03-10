#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "mt19937ar/mt19937ar.h"

void printA(int[], int size);
int int_cmp(const void *, const void *);

int main()
{
   int size = 20000;
   int a[size];

   init_genrand( time(NULL) );
   srand(time(NULL));
   int i;
   for(i=0; i<size; i++)
      a[i] = genrand_int32()%64;

   printf("time is: %#lx\n", time(NULL));

   qsort(a, size, sizeof(int), int_cmp);
   printA(a, size);
   printf("\n");

   return 0;
}

void printA(int a[], int size)
{
   printf("[");

   int i;
   for(i=0; i<size; i++){
      printf("%d", a[i]);
      if(i==size-1) break;
      printf(", ");
   }

   printf("]");
}

/* qsort int comparison function */
int int_cmp(const void *a, const void *b)
{
    const int *ia = (const int *)a; // casting pointer types
    const int *ib = (const int *)b;
    return *ia  - *ib;
	/* integer comparison: returns negative if b > a
	and positive if a > b */
}

