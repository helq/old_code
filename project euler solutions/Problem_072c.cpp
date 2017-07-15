#include <iostream>
using namespace std;
// Time solution: ~97 min

long farey(int n)
{
    int a, b, c, d, k, _0, _1;
    long i;

    a = 0;
    b = 1;
    c = 1;
    d = n;
    i = 1;

    while (c<=n)
    {
        k = int(long(n+b)/ long(d));
        _0 = c;
        _1 = d;
        c = ((k*c)-a);
        d = ((k*d)-b);
        a = _0;
        b = _1;

        i++;
    }
    return i;
}

int main()
{
    cout<< farey(1000000)-2 <<endl;

    return 0;
}