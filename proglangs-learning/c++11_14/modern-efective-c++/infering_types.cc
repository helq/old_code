//NOLINT(legal/copyright)
#include <iostream>

template<typename T>
void f(T&& param, T&& param2) {
  param = param2;
}

int main(int argc, char const* argv[])
{
  int x = 23;
  int y = 12;

  std::cout << x << " " << y << std::endl;

  f(x, y);

  std::cout << x << " " << y << std::endl;
  return 0;
}
