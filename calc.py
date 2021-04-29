#  Arithmetic expressions based on rational numbers

import ply.yacc as yacc
import ply.lex as lex
tokens = (
    'NAME', 'NUMBER',
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'EQUALS', 'DIVIDER',
    'LPAREN', 'RPAREN',
)

# Tokens

t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_EQUALS = r'='
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_DIVIDER = r'\|'


def t_NUMBER(t):
    r'\d+'
    t.value = str(t.value)
    return t


# Ignored characters
t_ignore = " \t"


def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")


def t_error(t):
    print(f"Illegal character {t.value[0]!r}")
    t.lexer.skip(1)


# Build the lexer
lex.lex()

# Precedence rules for the arithmetic operators
precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
    ('right', 'UMINUS'),
    ('left', 'DIVIDER'),
)


def p_statement_expr(p):
    'statement : expression'
    print(p[1])


def p_expression_binop(p):
    '''expression : expression PLUS expression
                  | expression MINUS expression
                  | expression TIMES expression
                  | expression DIVIDE expression
                  | expression DIVIDER expression'''

    if '|' in p[1]:
        print(p[1].split('|'))
        if p[2] == '+':
            p[0] = p[1] + p[3]
        elif p[2] == '-':
            p[0] = p[1] - p[3]
        elif p[2] == '*':
            p[0] = p[1] * p[3]
        elif p[2] == '/':
            p[0] = p[1] / p[3]
        elif p[2] == '|':
            p[0] = p[1] + p[2] + p[3]
    elif '|' in p[3]:
        print(p[3].split('|'))
        if p[2] == '+':
            p[0] = p[1] + p[3]
        elif p[2] == '-':
            p[0] = p[1] - p[3]
        elif p[2] == '*':
            p[0] = p[1] * p[3]
        elif p[2] == '/':
            p[0] = p[1] / p[3]
        elif p[2] == '|':
            p[0] = p[1] + p[2] + p[3]
    elif '|' in p[2]:
        p[0] = p[1] + p[2] + p[3]
    else:
        p[1] = int(p[1])
        p[3] = int(p[3])
        if p[2] == '+':
            p[0] = str(p[1] + p[3])
        elif p[2] == '-':
            p[0] = str(p[1] - p[3])
        elif p[2] == '*':
            p[0] = str(p[1] * p[3])
        elif p[2] == '/':
            p[0] = str(p[1] / p[3])


def p_expression_uminus(p):
    'expression : MINUS expression %prec UMINUS'
    p[0] = -p[2]


def p_expression_group(p):
    'expression : LPAREN expression RPAREN'
    p[0] = p[2]


def p_expression_number(p):
    'expression : NUMBER'
    p[0] = p[1]


def p_error(p):
    print(f"Syntax error at {p.value!r}")


yacc.yacc()

while True:
    try:
        s = input('calc > ')
    except EOFError:
        break
    yacc.parse(s)
