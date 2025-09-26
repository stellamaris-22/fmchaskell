#include <iostream>
#include <string>


int main(){
    std::string num;
    std::getline(std::cin, num);

    int count{0};
    for(auto c : num){
        switch (c){
            case 'S': ++count; break;
            case 'O': std::cout<<count<<std::endl; return 0;
            default: continue;
        }
    }
}