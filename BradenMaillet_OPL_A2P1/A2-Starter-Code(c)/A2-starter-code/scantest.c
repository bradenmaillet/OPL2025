#include <stdio.h>
#include "scan.h"

const char* names[] = {"read", "write", "id", "literal", "gets",
                       "add", "sub", "mul", "div", "lparen", "rparen", "eof"};

static token input_token;

int main() {

    do {
        input_token = scan ();
        printf ("scanned %s", names[input_token]);
        if (input_token == t_id || input_token == t_literal)
            printf (": %s", token_image);
        printf ("\n");
    } while (input_token != t_eof);

    return 0;
}
