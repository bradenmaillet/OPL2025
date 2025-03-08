/* Complete recursive descent parser for the calculator language.
    Builds on figure 2.16.  Prints a trace of productions predicted and
    tokens matched.  Does no error recovery: prints "syntax error" and
    dies on invalid input.
    Michael L. Scott, 2008-2020.
*/
#include <vector>
#include <string>
#include <iostream>
#include <exception>
#include "stdio.h"
#include "stdlib.h"

#include "scan.hpp"

class SyntaxError : public std::exception {
    public:
        SyntaxError() : _ErrMsg("DEBUG: Syntax Error"){}
        SyntaxError(const std::string& ErrMsg) : _ErrMsg(ErrMsg) {}
        const char* what() const noexcept override{
            return _ErrMsg.c_str();
        }
    private:
        std::string _ErrMsg;
};

std::ostream& operator<<(std::ostream& os, const token Token)
{
  switch(Token)
  {
    case token::t_read:
        os << "t_read";
        break;
    case token::t_write:
        os << "t_write";
        break;
    case token::t_id:
        os << "t_id";
        break;
    case token::t_literal:
        os << "t_literal";
        break;
    case token::t_gets:
        os << "t_gets";
        break;
    case token::t_add:
        os << "t_add";
        break;
    case token::t_sub:
        os << "t_sub";
        break;
    case token::t_mul:
        os << "t_mul";
        break;
    case token::t_div:
        os << "t_div";
        break;
    case token::t_lparen:
        os << "t_lparen";
        break;
    case token::t_rparen:
        os << "t_rparen";
        break;
    case token::t_eof:
        os << "t_eof";
        break;
    case token::t_if:
        os << "t_if";
        break;
    case token::t_while:
        os << "t_while";
        break;
    case token::t_end:
        os << "t_end";
        break;
    case token::t_eq:
        os << "t_eq";
        break;
    case token::t_neq:
        os << "t_neq";
        break;
    case token::t_lt:
        os << "t_lt";
        break;
    case token::t_lte:
        os << "t_lte";
        break;
    case token::t_gt:
        os << "t_gt";
        break;
    case token::t_gte:
        os << "t_gte";
        break;
    default:
        break;
  }
  return os;
}

const std::vector<std::string> names = {"read", "write",
     "id", "literal", "gets","add", "sub", "mul", "div", "lparen", "rparen", "eof", 
    "if" , "while", "end", "eq", "neq", "lt", "lte", "gt", "gte"};

/*const char* names[] = {"read", "write", "id", "literal", "gets",
                       "add", "sub", "mul", "div", "lparen", "rparen", "eof"};*/

static token input_token;

void error (const std::string& func) {
    std::cerr << "DEBUG: syntax error: " << func << std::endl;
    return;
}

void match (token expected) {
    if (input_token == expected) {
        std::cout << "matched " << names.at(input_token);
        if (input_token == t_id || input_token == t_literal)
            std::cout << ": " << token_image;
        std::cout << std::endl;
        input_token = scan ();
    } else {
        //error ("match");
        //input_token = expected;
        std::cerr << "DEBUG: expected token: " << expected << std::endl;
        std::cerr << "DEBUG: found token: " << input_token << std::endl;
        throw SyntaxError("DEBUG: Syntax Error: match()");
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
void cond ();
void rel_op ();


void program () {
    try{
        switch (input_token) {
            case t_id:
            case t_read:
            case t_write:
            case t_if:
            case t_while:
            case t_eof:
                std::cout << "predict program --> stmt_list eof" << std::endl;
                stmt_list ();
                match (t_eof);
                break;
            default: throw SyntaxError("DEBUG: Syntax Error: Program()");
        }
    } catch (const SyntaxError& e) {
        std::cerr << e.what() << " detected, attempting to recover" << std::endl;
        while(true){
            switch(input_token) {
                case t_id:      //first case
                case t_read:
                case t_write:
                case t_if:
                case t_while:
                case t_eof:
                    program();
                    return;

                default:  //  follow case
                    std::cerr << "DEBUG: Token Skipped: " << input_token << std::endl;
                    input_token = scan();
            }
        }
    }
}

void stmt_list () {
    try {
        switch (input_token) {
            case t_id:
            case t_read:
            case t_write:
            case t_if:
            case t_while:
                std::cout << "predict stmt_list --> stmt stmt_list" << std::endl;
                stmt ();
                stmt_list ();
                break;
            case t_end:
            case t_eof:
                std::cout << "predict stmt_list --> epsilon" << std::endl;
                break;          /* epsilon production */
            default: throw SyntaxError("DEBUG: Syntax Error: Stmt_list()");
        } 
    } catch(const SyntaxError& e) {
        std::cerr << e.what() << " detected, attempting to recover" << std::endl;
        while(true){
            switch(input_token){
                case t_id:
                case t_read:
                case t_write:
                case t_if:
                case t_while:
                    std::cerr << "DEBUG: Recovered, found item in first set" << std::endl;
                    stmt_list();
                    return;
                case t_end:
                case t_eof:
                    std::cerr << "DEBUG: Recovered, via skipping to follow set" << std::endl;
                    return;
                default:
                    std::cerr << "DEBUG: Token Skipped: " << input_token << std::endl;
                    input_token = scan();
                    break;
            }
        }
    }
}

void stmt () {
    try{
        switch (input_token) {
            case t_id:
                std::cout << "predict stmt --> id gets expr" << std::endl;
                match (t_id);
                match (t_gets);
                expr ();
                break;
            case t_read:
                std::cout << "predict stmt --> read id" << std::endl;
                match (t_read);
                match (t_id);
                break;
            case t_write:
                std::cout << "predict stmt --> write expr" << std::endl;
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
            default: throw SyntaxError("DEBUG: Syntax Error: stmt()");
        }
    } catch(const SyntaxError& e) {
        std::cerr << e.what() << " detected, attempting to recover" << std::endl;
        while(true){
            switch(input_token){
                case t_id:  //  first case
                case t_read:
                case t_write:
                case t_if:
                case t_while:
                    std::cerr << "DEBUG: Recovered, found item in first set" << std::endl;
                    stmt();
                    return;
                case t_eof:
                    std::cerr << "DEBUG: Recovered, via skipping to follow set" << std::endl;
                    return;
                default:
                    std::cerr << "DEBUG: Token Skipped: " << input_token << std::endl;
                    input_token = scan();
                    break;
            }
        }
    }
}

void expr () {
    try{
        switch (input_token) {
            case t_id:
            case t_literal:
            case t_lparen:
                std::cout << "predict expr --> term term_tail" << std::endl;
                term ();
                term_tail ();
                break;
            default: throw SyntaxError("DEBUG: Syntax Error: expr()");
        }
    }catch(const SyntaxError& e){
        std::cerr << e.what() << " detected, attempting to recover" << std::endl;
        while(true){
            switch(input_token){
                case t_lparen:
                case t_id:
                case t_literal:
                    std::cerr << "DEBUG: Recovered, found item in first set" << std::endl;
                    expr();
                    return;
                case t_rparen:
                case t_read:
                case t_write:
                case t_eof:
                    std::cerr << "DEBUG: Recovered, via skipping to follow set" << std::endl;
                    return;
                default:
                    std::cerr << "DEBUG: Token Skipped: " << input_token << std::endl;
                    input_token = scan();
                    break;
            }
        }
    }
}

void term () {
    try{
        switch (input_token) {
            case t_id:
            case t_literal:
            case t_lparen:
                std::cout << "predict term --> factor factor_tail" << std::endl;
                factor ();
                factor_tail ();
                break;
            default: throw SyntaxError("DEBUG: Syntax Error: term()");
        }
    }catch(const SyntaxError& e){
        std::cerr << e.what() << " detected, attempting to recover" << std::endl;
        while(true){
            switch(input_token){
                case t_lparen:
                case t_id:
                case t_literal:
                    std::cerr << "DEBUG: Recovered, found item in first set" << std::endl;
                    term();
                    return;
                case t_add:
                case t_sub:
                case t_rparen:
                case t_read:
                case t_write:
                case t_eof:
                    std::cerr << "DEBUG: Recovered, via skipping to follow set" << std::endl;
                    return;
                default:
                    std::cerr << "DEBUG: Token Skipped: " << input_token << std::endl;
                    input_token = scan();
                    break;
            }
        }
    }
}

void term_tail () {
    try{
        switch (input_token) {
            case t_add:
            case t_sub:
                std::cout << "predict term_tail --> add_op term term_tail" << std::endl;
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
                break;          /* epsilon production */
            default: throw SyntaxError("DEBUG: Syntax Error: term_tail()");
        }
    }catch(const SyntaxError& e){
        std::cerr << e.what() << " detected, attempting to recover" << std::endl;
        while(true){
            switch(input_token){
                case t_add:
                case t_sub:
                    std::cerr << "DEBUG: Recovered, found item in first set" << std::endl;
                    term_tail();
                    return;
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
                    std::cerr << "DEBUG: Recovered, via skipping to follow set" << std::endl;
                    return;
                default:
                    std::cerr << "DEBUG: Token Skipped: " << input_token << std::endl;
                    input_token = scan();
                    break;
            }
        }
    }
}

void factor () {
    try{
        switch (input_token) {
            case t_literal:
                std::cout << "predict factor --> literal" << std::endl;
                match (t_literal);
                break;
            case t_id :
                std::cout << "predict factor --> id" << std::endl;
                match (t_id);
                break;
            case t_lparen:
                std::cout << "predict factor --> lparen expr rparen" << std::endl;  
                match (t_lparen);
                expr ();
                match (t_rparen);
                break;
            default: throw SyntaxError("DEBUG: Syntax Error: factor()");
        }
    } catch(const SyntaxError& e) {
        std::cerr << e.what() << " detected, attempting to recover" << std::endl;
        while(true){
            switch(input_token){ // first case
                case t_lparen:
                case t_id:
                case t_literal:
                    std::cerr << "DEBUG: Recovered, found item in first set" << std::endl;
                    factor();
                    return;
                case t_add:     // follow case
                case t_sub:
                case t_mul:
                case t_div:
                case t_rparen:
                case t_read:
                case t_write:
                case t_eof:
                    std::cerr << "DEBUG: Recovered, via skipping to follow set" << std::endl;
                    return;
                default:
                    std::cerr << "DEBUG: Token Skipped: " << input_token << std::endl;
                    input_token = scan();
                    break;
            }
        }
    }
}

void factor_tail () {
    try{
        switch (input_token) {
            case t_mul:
            case t_div:
                std::cout << "predict factor_tail --> mul_op factor factor_tail" << std::endl;
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
                break;          /* epsilon production */
            default: throw SyntaxError("DEBUG: Syntax Error: factor_tail()");
        }
    } catch(const SyntaxError& e) {
        std::cerr << e.what() << " detected, attempting to recover" << std::endl;
        while(true){
            switch(input_token){ // first case
                case t_mul:
                case t_div:
                    std::cerr << "DEBUG: Recovered, found item in first set" << std::endl;
                    factor_tail();
                    return;
                case t_add:  //  follow
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
                    std::cerr << "DEBUG: Recovered, via skipping to follow set" << std::endl;
                    return;
                default:
                    std::cerr << "DEBUG: Token Skipped: " << input_token << std::endl;
                    input_token = scan();
                    break;
            }
        }
    }
}

void add_op () {
    try{
        switch (input_token) {
            case t_add:
                std::cout << "predict add_op --> add" << std::endl;
                match (t_add);
                break;
            case t_sub:
                std::cout << "predict add_op --> sub" << std::endl;
                match (t_sub);
                break;
            default: SyntaxError("DEBUG: Syntax Error: add_op()");
        }
    } catch(const SyntaxError& e) {
        std::cerr << e.what() << " detected, attempting to recover" << std::endl;
        while(true){
            switch(input_token){ // first case
                case t_add:
                case t_sub:
                    std::cerr << "DEBUG: Recovered, found item in first set" << std::endl;
                    add_op();
                    return;
                case t_lparen:  //  follow
                case t_id:
                case t_literal:
                    std::cerr << "DEBUG: Recovered, via skipping to follow set" << std::endl;
                    return;
                default:
                    std::cerr << "DEBUG: Token Skipped: " << input_token << std::endl;
                    input_token = scan();
                    break;
            }
        }
    }
}

void mul_op () {
    try{
        switch (input_token) {
            case t_mul:
                std::cout << "predict mul_op --> mul" << std::endl;
                match (t_mul);
                break;
            case t_div:
                std::cout << "predict mul_op --> div" << std::endl;
                match (t_div);
                break;
            default: SyntaxError("DEBUG: Syntax Error: mul_op()");
        }
    } catch(const SyntaxError& e) {
        std::cerr << e.what() << " detected, attempting to recover" << std::endl;
        while(true){
            switch(input_token){ // first case
                case t_mul:
                case t_div:
                    std::cerr << "DEBUG: Recovered, found item in first set" << std::endl;
                    mul_op();
                    return;
                case t_lparen:  //  follow
                case t_id:
                case t_literal:
                    std::cerr << "DEBUG: Recovered, via skipping to follow set" << std::endl;
                    return;
                default:
                    std::cerr << "DEBUG: Token Skipped: " << input_token << std::endl;
                    input_token = scan();
                    break;
            }
        }
    }
}
void cond (){
    try{
        switch (input_token) {
            case t_id:
            case t_literal:
            case t_lparen:
                std::cout << "predict cond --> expr rel_op expr" << std::endl;
                expr();
                rel_op();
                expr();
                break;
            default: SyntaxError("Syntax Error: cond()");
        }
    } catch(const SyntaxError& e) {
        std::cerr << e.what() << " detected, attempting to recover" << std::endl;
        while(true){
            switch(input_token){ // first case
                case t_lparen:
                case t_id:
                case t_literal:
                    std::cerr << "DEBUG: Recovered, found item in first set" << std::endl;
                    cond();
                    return;
                case t_rparen:  //  follow
                case t_read:
                case t_write:
                case t_if:
                case t_while:
                case t_eof:
                    std::cerr << "DEBUG: Recovered, via skipping to follow set" << std::endl;
                    return;
                default:
                    std::cerr << "DEBUG: Token Skipped: " << input_token << std::endl;
                    input_token = scan();
                    break;
            }
        }
    }
}
void rel_op (){
    try{
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
                match (input_token);
                break;
            default: throw SyntaxError("DEBUG: Syntax Error: cond()");
        }
    } catch(const SyntaxError& e) {
        std::cerr << e.what() << " detected, attempting to recover" << std::endl;
        while(true){
            switch(input_token){
                case t_eq:
                case t_neq:
                case t_lt:
                case t_lte:
                case t_gt:
                case t_gte:
                    std::cerr << "DEBUG: Recovered, found item in first set" << std::endl;
                    rel_op();
                    return;
                case t_lparen:  //  follow
                case t_id:
                case t_literal:
                    std::cerr << "DEBUG: Recovered, via skipping to follow set" << std::endl;
                    return;
                default:
                    std::cerr << "DEBUG: Token Skipped: " << input_token << std::endl;
                    input_token = scan();
                    break;
            }
        }
    }
}

int main () {
    input_token = scan ();
    program ();
    return 0;
}
