#include <stdio.h>
#include <iostream>
#include <vector>
#include <string>
#include "scan.hpp"



const std::vector<std::string> names = {"read", "write",
     "id", "literal", "gets","add", "sub", "mul", "div", "lparen", "rparen", "eof", 
    "if" , "while", "end", "eq", "neq", "lt", "lte", "gt", "gte"};

static token input_token;

int main() {
    do {
        input_token = scan ();
        std::cout << "scanned " << names.at(input_token);
        if (input_token == t_id || input_token == t_literal)
            std::cout << ": " << token_image;
        std::cout << std::endl;
    } while (input_token != t_eof);

    return 0;
}
