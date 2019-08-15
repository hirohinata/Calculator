grammar Expression;

input
    : expr EOF
    ;

expr
    : additiveExpr
    ;
additiveExpr
    : multipricativeExpr                                #noneAdditiveExpr
    | lhs=additiveExpr PLUS rhs=multipricativeExpr      #addExpr
    | lhs=additiveExpr MINUS rhs=multipricativeExpr     #subExpr
    ;
multipricativeExpr
    : powerExpr                                         #noneMultipricativeExpr
    | lhs=multipricativeExpr ASTERISK rhs=powerExpr     #multiExpr
    | lhs=multipricativeExpr SLASH rhs=powerExpr        #divExpr
    | lhs=multipricativeExpr rhs=parenExpr              #parenMultiExpr
    ;
powerExpr
    : unaryExpr                                         #nonePowerExpr
    | lhs=unaryExpr HAT rhs=powerExpr                   #powExpr
    ;
unaryExpr
    : primaryExpr                                       #noneUnaryExpr
    | PLUS rhs=unaryExpr                                #plusExpr
    | MINUS rhs=unaryExpr                               #minusExpr
    ;
primaryExpr
    : parenExpr
    | funccallExpr
    | literal
    ;
parenExpr
    : OPEN_PAREN expr CLOSE_PAREN
    ;
funccallExpr
    : funcname=IDENTIFIER OPEN_PAREN (args+=expr (COMMA args+=expr)*)? CLOSE_PAREN
    ;
literal
    : UINT      #uintLiteral
    | REAL      #realLiteral
    | STRING    #stringLiteral
    ;

PLUS: '+';
MINUS: '-';
ASTERISK: '*';
SLASH: '/';
HAT: '^';
OPEN_PAREN: '(';
CLOSE_PAREN: ')';
COMMA: ',';
DOT: '.';

UINT: [0-9]+;
REAL: UINT DOT UINT? | DOT UINT;
STRING: '"' (~[^"] | '""')* '"';

fragment ID_START: [A-Za-z_];
fragment ID_CONTINUE: ID_START | [0-9];
IDENTIFIER: ID_START ID_CONTINUE*;

WS: [ \t]+ -> skip;