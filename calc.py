#  Arithmetic expressions based on rational numbers

import ply.yacc as yacc
import ply.lex as lex
tokens = (
    'NAME', 'NUMBER',
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'DIVIDER',
    'LPAREN', 'RPAREN',
)

# Tokens

t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_DIVIDER = r'\|'


def gcd(a, b): return gcd(b, a % b) if b else a


def fractions_shortening(numerator, denominator):
    divider = gcd(numerator, denominator)
    return numerator/divider, denominator/divider


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
    if '|' in p[1] and '|' in p[3]:
        split1 = p[1].split('|')
        numerator1 = int(split1[0])
        denominator1 = int(split1[1])
        split2 = p[3].split('|')
        numerator2 = int(split2[0])
        denominator2 = int(split2[1])
        if p[2] == '+':
            if denominator1 == denominator2:
                r_numerator = numerator1+numerator2
                r_denominator = denominator1
                r_numerator, r_denominator = fractions_shortening(
                    r_numerator, r_denominator)
                p[0] = str(numerator1+numerator2)+'|'+str(denominator1)
            else:
                p[0] = str(numerator1*denominator2+numerator2 *
                           denominator1)+'|'+str(denominator1*denominator2)
        elif p[2] == '-':
            if denominator1 == denominator2:
                p[0] = str(numerator1+numerator2)+'|'+str(denominator1)
            else:
                p[0] = str(numerator1*denominator2-numerator2 *
                           denominator1)+'|'+str(denominator1*denominator2)
        elif p[2] == '*':
            p[0] = str(numerator1*numerator2)+'|' + \
                str(denominator1*denominator2)
        elif p[2] == '/':
            p[0] = str(numerator1*denominator2)+'|' + \
                str(denominator1*numerator2)
    elif '|' in p[1]:
        split = p[1].split('|')
        numerator = int(split[0])
        denominator = int(split[1])
        number = int(p[3])
        if p[2] == '+':
            p[0] = str(numerator+denominator*number)+'|'+str(denominator)
        elif p[2] == '-':
            p[0] = str(numerator-denominator*number)+'|'+str(denominator)
        elif p[2] == '*':
            p[0] = str(numerator*number)+'|'+str(denominator)
        elif p[2] == '/':
            p[0] = str(numerator)+'|'+str(denominator*number)
    elif '|' in p[3]:
        split = p[3].split('|')
        numerator = int(split[0])
        denominator = int(split[1])
        number = int(p[1])
        if p[2] == '+':
            p[0] = str(numerator+denominator*number)+'|'+str(denominator)
        elif p[2] == '-':
            p[0] = str(numerator-denominator*number)+'|'+str(denominator)
        elif p[2] == '*':
            p[0] = str(numerator*number)+'|'+str(denominator)
        elif p[2] == '/':
            p[0] = str(numerator)+'|'+str(denominator*number)
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
