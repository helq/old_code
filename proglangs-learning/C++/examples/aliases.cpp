#include <iostream>

int main() {
    int n;
    int& r = n;

    r = 10;
    std::cout << "n = " << n << std::endl;

    n = -2;
    std::cout << "r = " << r << std::endl;

    return 0;
}
