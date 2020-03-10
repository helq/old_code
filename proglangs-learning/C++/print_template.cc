#include <iostream>
#include "print_template.hh"

struct point {
  int x;
  int y;

  void print(std::ostream& os) const {
    os << "(" <<  this->x << "," << this->y << ")";
  }
};

int main(int argc, char const* argv[])
{
  point a = {15, 20};

  //a.print( std::cout );
  std::cout << a << std::endl;
  return 0;
}
