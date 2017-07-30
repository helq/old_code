// NOLINT(legal/copyright)
#include <array>
#include <functional>
#include <iostream>

#include "util.h"

using std::array;
using std::function;

template<typename T>
class Base {
  public:
    Base() = default;
    using Function = function<T()>;
};

template<std::size_t N>
class Derived : public Base<array<double,N>> {
  // function type declared in Base
  using Function = typename Derived::Function;

  array<double,N> fun;
  public:
    explicit Derived(Function f) : fun(f()) {}
    array<double,N> returnVector() { return fun; }
};

int main()
{
  auto myfun = []() {
    array<double,3> v = {1, 2, 3};
    return v;
  };

  Derived<3> d{myfun};

  print_array( d.returnVector() );
  
  return 0;
}
