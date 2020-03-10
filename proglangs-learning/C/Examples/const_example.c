#include <stdio.h>

void modify(int[]);
/*void noModify(const int[]);*/ 

int main()
{
   const int i=1;
   
   /*printf("%d\n", ++i); // no es posible hacerlo, no compila */
   printf("%d\n", i);
   
   int j[] = {12, 2};
   modify(j);
   printf("[%d, %d]\n", j[0], j[1]);
   
   return i;
}

void modify(int i[]) {
   i[0]=0;
}

/*void noModify(const int i[]) {
   i[0]=0; // No se puede realizar esta operación, se ha tomado a i[] como const
}*/ 
