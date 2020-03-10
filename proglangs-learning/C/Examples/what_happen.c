#include <stdio.h>

int main()
{
   printf("%s: -1L < 1U in this machine\n", -1L < 1U ? "YES": "NOT");

   long a = -1;
   unsigned int b = 1;

   printf("%s: -1L < (unsigned long)1U in this machine\n",
                                             a < (unsigned long)b ? "YES": "NOT");

   return 0;
}

