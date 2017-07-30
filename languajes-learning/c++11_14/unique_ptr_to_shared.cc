// NOLINT(legal/copyright)
#include <iostream>
#include <memory>

using std::shared_ptr;
using std::make_unique;

struct widget {
	int a;
};

int main() {

    auto uptr = make_unique<widget>();
    uptr->a = 2;
    std::cout << uptr->a << std::endl;
    
    shared_ptr<widget> sptr(std::move(uptr));
    std::cout << sptr->a << std::endl;
    if(uptr) {
    	uptr->a = 4;
        std::cout << uptr->a << std::endl;
    }
}
