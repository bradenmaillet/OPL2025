/* Simple ad-hoc scanner for the calculator language.
    Michael L. Scott, 2008-2020.
*/

#include <iostream>
#include <string>
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "ctype.h"

#include "scan.hpp"

std::string token_image;

token scan() {
    token_image.clear();
    static int c = ' ';
        /* next available char; extra (int) width accommodates EOF */
    int i = 0;              /* index into token_image */

    /* skip white space */
    while (isspace(c)) {
        c = getc(stdin);
    }
    if (c == EOF)
        return t_eof;
    if (isalpha(c)) {
        do {
            token_image.push_back(c);
            if (i >= MAX_TOKEN_LEN) {
                std::cout << "max token length exceeded" << std::endl;
                exit(1);
            }
            c = getc(stdin);
        } while (isalpha(c) || isdigit(c) || c == '_');
        //token_image[i] = '\0';
        if (!token_image.compare("read")) return t_read;
        else if (!token_image.compare("write")) return t_write;
        else if(!token_image.compare("if")) return t_if;
        else if(!token_image.compare("while")) return t_while;
        else if(!token_image.compare("end")) return t_end;
        else return t_id;
    }
    else if (isdigit(c)) {
        do {
            token_image.push_back(c);
            c = getc(stdin);
        } while (isdigit(c));
        //token_image[i] = '\0';
        return t_literal;
    } else switch (c) { // `<`, `<=`, `>`, `>=`, `=`, and `!=`
        case ':':
            if ((c = getc(stdin)) != '=') {
                std::cerr << "error" << std::endl;
                exit(1);
            } else {
                c = getc(stdin);
                return t_gets;
            }
            break;
        case '!':
            if ((c = getc(stdin)) != '=') {
                std::cerr << "error" << std::endl;
                exit(1);
            } else {
                c = getc(stdin);
                return t_neq;
            }
            break;
        case '<':
            if ((c = getc(stdin)) == ' ') {
                c = getc(stdin);
                return t_lt;
            } else if (c == '='){
                c = getc(stdin);
                return t_lte;
            } else {
                std::cerr << "error" << std::endl;
                exit(1);
            }
            break;
        case '>':
            if ((c = getc(stdin)) == ' ') {
                c = getc(stdin);
                return t_gt;
            } else if (c == '='){
                c = getc(stdin);
                return t_gte;
            } else {
                std::cerr << "error" << std::endl;
                exit(1);
            }
            break;
        case '(': c = getc(stdin); return t_lparen;
        case ')': c = getc(stdin); return t_rparen;
        case '+': c = getc(stdin); return t_add;
        case '-': c = getc(stdin); return t_sub;
        case '*': c = getc(stdin); return t_mul;
        case '/': c = getc(stdin); return t_div;
        case '=': c = getc(stdin); return t_eq;
        default:
            std::cerr << "error" << std::endl;
            exit(1);
    }
}
