#include <iostream>
using namespace std;

//Tiempo que tardó la ejecución 10m46.621s

int number_divisors(long number)
{
    int total_divisors = 0;
    int i;
    for (i = 1; i <= (number / 2) + 1; i++)
    {
        if(number % i == 0)
            total_divisors++;
    }
    return total_divisors + 1;
}

int main()
{
	int i = 0;
	int number = 500;
	long triangle_number = 0;
    
	while(true)
	{
		i++;
		triangle_number += i;
		if (number <= number_divisors(triangle_number))
		{
			cout<<triangle_number<<endl;
			break;
	    }
	}
}

