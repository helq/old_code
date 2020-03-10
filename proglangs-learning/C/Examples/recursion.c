// Original in: http://rosettacode.org/wiki/Category:C
// With GCC 4.5, if compiled without -O2, it segfaults quickly;
// if gcc -O2, crash never happens, because the optimizer noticed
// the tail recursion in recur() and turned it into a loop!

#include <stdio.h>

char * base;

void get_diff();
void recur();

int main()
{
   char v = 32;
   printf("pos of v: %p\n", base = &v);
   recur();
   return 0;
}

void recur()
{
   get_diff();
   recur();
}

void get_diff()
{
   char x;
   if (base - &x < 500) // only print the first elements
      printf("%p %d\n", &x, (int)(base - &x));
}
