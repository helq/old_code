// taken from: https://stackoverflow.com/a/35811860
template<class T>
auto operator<<(std::ostream& os, const T& t) -> decltype(t.print(os), os) 
{ 
    t.print(os); 
    return os; 
} 
