/* Complete recursive descent parser for the calculator language.
    Builds on figure 2.16.  Prints a trace of productions predicted and
    tokens matched.  Does no error recovery: prints "syntax error" and
    dies on invalid input.
    Michael L. Scott, 2008-2020.
*/
#include <vector>
#include <string>
#include <iostream>
#include "stdio.h"
#include "stdlib.h"

#include "scan.hpp"

const std::vector<std::string> names = {"read", "write",
     "id", "literal", "gets","add", "sub", "mul", "div", "lparen", "rparen", "eof", 
    "if" , "while", "end", "eq", "neq", "lt", "lte", "gt", "gte"};

/*const char* names[] = {"read", "write", "id", "literal", "gets",
                       "add", "sub", "mul", "div", "lparen", "rparen", "eof"};*/

static token input_token;

void error (const std::string& func) {
    std::cerr << "syntax error: " << func << std::endl;
    //printf ("syntax error\n");
    exit (1);
}

void match (token expected) {
    if (input_token == expected) {
        std::cout << "matched " << names.at(input_token);
        //printf ("matched %s", names[input_token]);
        if (input_token == t_id || input_token == t_literal)
            std::cout << ": " << token_image;
            //printf (": %s", token_image);
        std::cout << std::endl;
        //printf ("\n");
        input_token = scan ();
    }
    else 
    {
        //printf("error called in match\n");
        error ("match");
    }
}

void program ();
void stmt_list ();
void stmt ();
void expr ();
void term ();
void term_tail ();
void factor ();
void factor_tail ();
void add_op ();
void mul_op ();
//new funcs
void cond ();
void rel_op ();


void program () {
    switch (input_token) {
        case t_id:
        case t_read:
        case t_write:
        case t_eof:
            std::cout << "predict program --> stmt_list eof" << std::endl;
            //printf ("predict program --> stmt_list eof\n");
            stmt_list ();
            match (t_eof);
            break;
        default: error ("program");
    }
}

void stmt_list () {
    //std::cout << input_token << std::endl;
    switch (input_token) {
        case t_id:
        case t_read:
        case t_write:
            std::cout << "predict stmt_list --> stmt stmt_list" << std::endl;
            //printf ("predict stmt_list --> stmt stmt_list\n");
            stmt ();
            stmt_list ();
            break;
        case t_if:
            std::cout << "predict stmt_list --> stmt stmt_list" << std::endl;
            //printf ("predict stmt_list --> stmt stmt_list\n");
            stmt ();
            stmt_list ();
            break;
        case t_while:
            std::cout << "predict stmt_list --> stmt stmt_list" << std::endl;
            //printf ("predict stmt_list --> stmt stmt_list\n");
            stmt ();
            stmt_list ();
            break;
        case t_end:
        case t_eof:
            std::cout << "predict stmt_list --> epsilon" << std::endl;
            //printf ("predict stmt_list --> epsilon\n");
            break;          /* epsilon production */
        default: error ("stmt_list");
    }
}

void stmt () {
    switch (input_token) {
        case t_id:
            std::cout << "predict stmt --> id gets expr" << std::endl;
            //printf ("predict stmt --> id gets expr\n");
            match (t_id);
            match (t_gets);
            expr ();
            break;
        case t_read:
            std::cout << "predict stmt --> read id" << std::endl;
            //printf ("predict stmt --> read id\n");
            match (t_read);
            match (t_id);
            break;
        case t_write:
            std::cout << "predict stmt --> write expr" << std::endl;
            //printf ("predict stmt --> write expr\n");
            match (t_write);
            expr ();
            break;
        case t_if:
            std::cout << "predict stmt --> if cond stmt_list end" << std::endl;
            match(t_if);
            cond();
            stmt_list();
            match(t_end);
            break;
        case t_while:
            std::cout << "predict stmt --> while cond stmt_list end" << std::endl;
            match(t_while);
            cond();
            stmt_list();
            match(t_end);
            break;
        default: error ("stmt");
    }
}

void expr () {
    switch (input_token) {
        case t_id:
        case t_literal:
        case t_lparen:
            std::cout << "predict expr --> term term_tail" << std::endl;
            //printf ("predict expr --> term term_tail\n");
            term ();
            term_tail ();
            break;
        default: error ("expr");
    }
}

void term () {
    switch (input_token) {
        case t_id:
        case t_literal:
        case t_lparen:
            std::cout << "predict term --> factor factor_tail" << std::endl;
            //printf ("predict term --> factor factor_tail\n");
            factor ();
            factor_tail ();
            break;
        default: error ("term");
    }
}

void term_tail () {
    switch (input_token) {
        case t_add:
        case t_sub:
            std::cout << "predict term_tail --> add_op term term_tail" << std::endl;
            //printf ("predict term_tail --> add_op term term_tail\n");
            add_op ();
            term ();
            term_tail ();
            break;
        case t_rparen:
        case t_id:
        case t_read:
        case t_write:
        case t_if:
        case t_while:
        case t_end:
        case t_eq:
        case t_neq:
        case t_lt:
        case t_lte:
        case t_gt:
        case t_gte:
        case t_eof:
            std::cout << "predict term_tail --> epsilon" << std::endl;
            //printf ("predict term_tail --> epsilon\n");
            break;          /* epsilon production */
        default: error ("term_tail");
    }
}

void factor () {
    switch (input_token) {
        case t_literal:
            std::cout << "predict factor --> literal" << std::endl;
            //printf ("predict factor --> literal\n");
            match (t_literal);
            break;
        case t_id :
            std::cout << "predict factor --> id" << std::endl;
            //printf ("predict factor --> id\n");
            match (t_id);
            break;
        case t_lparen:
            std::cout << "predict factor --> lparen expr rparen" << std::endl;  
            //printf ("predict factor --> lparen expr rparen\n");
            match (t_lparen);
            expr ();
            match (t_rparen);
            break;
        default: error ("factor");
    }
}

void factor_tail () {
    switch (input_token) {
        case t_mul:
        case t_div:
            std::cout << "predict factor_tail --> mul_op factor factor_tail" << std::endl;
            //printf ("predict factor_tail --> mul_op factor factor_tail\n");
            mul_op ();
            factor ();
            factor_tail ();
            break;
        case t_add:
        case t_sub:
        case t_rparen:
        case t_id:
        case t_read:
        case t_write:
        case t_if:
        case t_while:
        case t_end:
        case t_eq:
        case t_neq:
        case t_lt:
        case t_lte:
        case t_gt:
        case t_gte:
        case t_eof:
            std::cout << "predict factor_tail --> epsilon" << std::endl;
            //printf ("predict factor_tail --> epsilon\n");
            break;          /* epsilon production */
        default: error ("factor_tail");
    }
}

void add_op () {
    switch (input_token) {
        case t_add:
            std::cout << "predict add_op --> add" << std::endl;
            //printf ("predict add_op --> add\n");
            match (t_add);
            break;
        case t_sub:
            std::cout << "predict add_op --> sub" << std::endl;
            //printf ("predict add_op --> sub\n");
            match (t_sub);
            break;
        default: error ("add_op");
    }
}

void mul_op () {
    switch (input_token) {
        case t_mul:
            std::cout << "predict mul_op --> mul" << std::endl;
            //printf ("predict mul_op --> mul\n");
            match (t_mul);
            break;
        case t_div:
            std::cout << "predict mul_op --> div" << std::endl;
            //printf ("predict mul_op --> div\n");
            match (t_div);
            break;
        default: error ("mul_op");
    }
}
void cond (){
    switch (input_token) {
        case t_id:
        case t_literal:
        case t_lparen:
            std::cout << "predict cond --> expr rel_op expr" << std::endl;
            expr();
            rel_op();
            expr();
            break;
        default: error ("cond");
    }
}
void rel_op (){
    switch (input_token) {
        case t_eq:
            std::cout << "predict rel_op --> eq" << std::endl;
            match (t_eq);
            break;
        case t_neq:
            std::cout << "predict rel_op --> neq" << std::endl;
            match (t_neq);
            break;
        case t_lt:
            std::cout << "predict rel_op --> lt" << std::endl;
            match (t_lt);
            break;
        case t_lte:
            std::cout << "predict rel_op --> lte" << std::endl;
            match (t_lte);
            break;
        case t_gt:
            std::cout << "predict rel_op --> gt" << std::endl;
            match (t_gt);
            break;
        case t_gte:
            std::cout << "predict rel_op --> gte" << std::endl;
            match (t_gte);
            break;
        default: error ("rel_op");
    }
}

int main () {
    input_token = scan ();
    program ();
    return 0;
}
