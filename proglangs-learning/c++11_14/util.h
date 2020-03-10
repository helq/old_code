// NOLINT(legal/copyright)
#include <array>
#include <iostream>
#include <iterator>
#include <vector>

// NOLINT(build/header_guard)
#ifndef UTIL_H
#define UTIL_H

template<typename T>
void print_vector(std::vector<T> v) {
  std::cout << "{";
  if(v.size()>0) {
    std::copy(v.begin(), v.end()-1, std::ostream_iterator<double>(std::cout, ", "));
    std::cout << v.back();
  }
  std::cout << "}" << std::endl;
}

template<typename T, std::size_t N>
void print_array(std::array<T,N> a) {
  std::cout << "{";
  if(a.size()>0) {
    std::copy(a.begin(), a.end()-1, std::ostream_iterator<double>(std::cout, ", "));
    std::cout << a.back();
  }
  std::cout << "}" << std::endl;
}

#endif // UTIL_H
