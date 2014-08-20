#include <iostream>
#include <string>
/**Declaring strings*/
float data[2];
std::string first_name = "Augusto";
std::string last_name  = "Ferrari";
std::string full_name  = first_name + " " + last_name;
char character ;


int main(){
	data[0]=1;
	data[1]=2;
	std::cout<< "data[0] = " << data[0]<<"\n";
	std::cout<< "data[1] = " << data[1]<<"\n";
	std::cout<< "My name is " << full_name<<"\n";
	
	/*to guet fist character of a array we do*/
	character = first_name.at(0);
	std::cout<< "the first character of "<< first_name << " is "<< character <<"\n";
	/*to get the length */
	std::cout<< "the length of " << full_name << " is " << full_name.length()<< "\n";

	return 0;
}