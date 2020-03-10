#include <stdio.h>
#include <ctype.h>

char q(int i){
   return i ? 'x' : '-';
}

int main()
{
   int i;

   char a, d, c, p, s;

   printf("\talpha\tdigit\tcntrl\tprint\tspace\n");
   for(i=0; i<128; i++){
      printf("%c es", i);
      a = q(isalpha(i));
      d = q(isdigit(i));
      c = q(iscntrl(i));
      p = q(isprint(i));
      s = q(isspace(i));
      printf("\t%c\t%c\t%c\t%c\t%c\n", a, d, c, p, s);
   }

   return 0;
}

