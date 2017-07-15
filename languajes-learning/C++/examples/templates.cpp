#include <iostream>

template <class T>
void swap(T &x, T &y) {
    T t = x; x = y; y = t;
}

int main() {
    int a = 1, b = 2;
    std::string c = "First", d = "Second";

    swap(a,b);
    swap(c,d);

    std::cout << a << " " << b << std::endl;
    std::cout << c << " " << d << std::endl;

    return 0;
}
