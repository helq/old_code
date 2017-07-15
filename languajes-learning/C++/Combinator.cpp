// compile with: g++ -std=c++11 Combinator.cpp
// by: Gadelan
// from: http://elmanantialdebits.blogspot.com/2011/10/el-combinador-y-en-c-y-ii.html

#include <functional>
#include <iostream>

// F(X,Y) es X->Y
#define F(X,Y) std::function<Y(X)>
 

// F(X,Y,Z) es (X,Y)->Z
#define F2(X,Y,Z) std::function<Z(X,Y)>

int g(F(int,int) f,int v) {
    return v==0 ? 1 : v * f(v-1);
}

template<typename T>
F(T,T) Y(  F2( F(T,T) ,T,T)  f)
{
    return [=](T x){return f(Y(f),x);};
}

int main(int argc,char** argv) {
    std::cout << Y<int>(g)(5) << std::endl;
    return 0;
}
