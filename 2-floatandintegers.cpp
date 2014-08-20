#include <iostream>

float floating,f1,f2;
int integer ;

int main(){
	floating= 1/3;
	integer  = 1/3;
	std::cout <<"float of "<<"1/3"<<"= "<< floating ;
	std::cout <<"\ninteger of "<<"1/3"<<"= "<< integer<<"\n"  ;
	floating= 1/2  + 1/2;
	integer  = 1/2  + 1/2;
	std::cout <<"float of "<<"1/2  + 1/2"<<"= "<< floating ;
	std::cout <<"\ninteger of "<<"1/2  + 1/2"<<"= "<< integer<<"\n"  ;
	floating= 3.0/2;
	integer  =3.0/2;
	std::cout <<"float of "<<"3.0/2"<<" = "<< floating ;
	std::cout <<"\ninteger of "<<"3.0/2"<<" = "<< integer<<"\n" ;

	return 0;
}