#include <stdio.h>
#include <stdlib.h>
 #include <time.h> 

void printA(int[], int size);

int main()
{
   int size = 20;
   int a[size];
   
   srand(time(NULL));
   int i;
   for(i=0; i<size; i++)
      a[i] = rand()%100;

   printf("time is: %#lx\n", time(NULL));
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
