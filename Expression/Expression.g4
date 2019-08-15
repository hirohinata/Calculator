grammar Expression;

input: expr EOF;    //Pythonで出力する人は、"input"という名前は使えないので変更すること

expr
    : num                                       #expr_none
    | paren_expr                                #expr_none
    | op=(PLUS|MINUS) expr                      #expr_unary
    | <assoc=right> lhs=expr HAT rhs=expr       #expr_power
    | lhs=expr op=(ASTERISK | SLASH) rhs=expr   #expr_multipricative
    | expr paren_expr                           #expr_multipricative
    | lhs=expr op=(PLUS|MINUS) rhs=expr         #expr_additive
    | funcname=IDENTIFIER OPEN_PAREN (args+=expr (COMMA args+=expr)*)? CLOSE_PAREN #expr_funccall   //+=を使うと、同じ種類の構文要素リスト化できる
    ;

paren_expr: OPEN_PAREN expr CLOSE_PAREN;
num
    : UINT
    ;

PLUS: '+';
MINUS: '-';
ASTERISK: '*';
SLASH: '/';
HAT: '^';
OPEN_PAREN: '(';
CLOSE_PAREN: ')';
COMMA: ',';

UINT: [0-9]+;

fragment ID_START: [A-Za-z_];
fragment ID_CONTINUE: ID_START | [0-9];
IDENTIFIER: ID_START ID_CONTINUE*;

WS: [ \t]+ -> skip;