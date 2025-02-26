#include <stdio.h>
#include <iostream>
#include <vector>
#include <string>
#include "scan.hpp"

const std::vector<std::string> names = {"read", "write", "id", "literal", "gets",
    "add", "sub", "mul", "div", "lparen", "rparen", "eof"};

/*const char* names[] = {"read", "write", "id", "literal", "gets",
                       "add", "sub", "mul", "div", "lparen", "rparen", "eof"};*/

static token input_token;

int main() {

    do {
        //std::cout << "--new scan--" << std::endl;
        input_token = scan ();
        //std::cout << "--scanned input_token: " << input_token << std::endl;
        std::cout << "scanned " << names.at(input_token);
        //printf ("scanned %s", names.at(input_token));
        if (input_token == t_id || input_token == t_literal)
            std::cout << ": " << token_image;
            //printf (": %s", token_image);
        std::cout << std::endl;
        //printf ("\n");
    } while (input_token != t_eof);

    return 0;
}
