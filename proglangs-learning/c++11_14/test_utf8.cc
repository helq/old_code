// NOLINT(legal/copyright)
#include <iostream>
#include <string>

int main()
{
  std::string testing("Some texñ");
  std::string testing_u(u8"Some texñ");
  std::cout << R"(String ")"         << testing   << R"( with size )" << testing.size() << std::endl;
  std::cout << R"(Unicode String ")" << testing_u << R"( with size )" << testing_u.size() << std::endl;
  return 0;
}
