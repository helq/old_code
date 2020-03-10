#include <functional>
#include <iostream>
#include <tuple>

// clang++ -std=c++17 -Wall -pedantic applying_tuple_to_function_functional_programming.cc -o applying_tuple_to_function_functional_programming && ./applying_tuple_to_function_functional_programming

using std::pair;
using std::function;
using std::tuple;

template<typename R, typename ... Args>
struct AppFun {
  function<R(Args...)> fun;
  explicit AppFun(function<R(Args...)>&& f) : fun{f} {}
  R apply(Args&&... params) { return fun(params ...); }
  R apply(tuple<Args...> params) { return std::apply(fun, params); }
};

pair<int,bool> myfun(int x, int y) {
  return {x+y, x<y};
}

int main(int argc, char const* argv[])
{
  AppFun test { (function<int(int, float)>) [](int x, float y) { return (int)y * x; } };

  std::cout << test.fun( 3, 4.2 )          // calling function directly
     << " " << test.apply( 3, 4.2 )        // calling function by passing parameters to apply
     << " " << test.apply( tuple{4, 5.7} ) // calling function with a tuple
     << " " << AppFun<pair<int,bool>, int, int> {myfun}.apply( tuple{3, 2} ).second
     << " " << AppFun<bool, int, int, int> { [](int,int,int){ return false; } }.apply( tuple{3, 2, 2} )
     << std::endl;

  return 0;
}
