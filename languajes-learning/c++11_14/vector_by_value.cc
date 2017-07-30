// NOLINT(legal/copyright)
#include <iostream>
#include <iterator>
#include <vector>

#include "util.h"

using std::vector;

void modify_vector_by_value(vector<double> v) {
  v[1] = 10.3;
}

void modify_vector_by_reference(vector<double>& v) {
  v[1] = 10.3;
}

int main(int argc, char const* argv[])
{
  vector<double> v = {0, 3, 6.2, 8};

  print_vector(v);
  modify_vector_by_value(v);
  print_vector(v);
  modify_vector_by_reference(v);
  print_vector(v);

  return 0;
}
