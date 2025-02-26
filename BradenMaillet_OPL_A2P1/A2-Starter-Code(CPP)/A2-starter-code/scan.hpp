/* Definitions the scanner shares with the parser
    Michael L. Scott, 2008-2020.
*/
#include <string>

typedef enum {t_read, t_write, t_id, t_literal, t_gets,
    t_add, t_sub, t_mul, t_div, t_lparen, t_rparen, t_eof} token;


#define MAX_TOKEN_LEN 128
extern std::string token_image;

extern token scan();
