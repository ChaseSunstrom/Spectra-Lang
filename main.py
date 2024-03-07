import ply.lex as lex
import ply.yacc as yacc

# Reserved words with specific type annotations added
reserved = {
    'if': 'IF', 'else': 'ELSE', 'while': 'WHILE', 'fn': 'FN',
    'return': 'RETURN', 'for': 'FOR', 'in': 'IN', 'true': 'TRUE',
    'false': 'FALSE', 'class': 'CLASS', 'struct': 'STRUCT',
    'enum': 'ENUM', 'use': 'USE', 'break': 'BREAK',
    'continue': 'CONTINUE', 'pub': 'PUB', 'priv': 'PRIV',
    'match': 'MATCH', 'interface': 'INTERFACE', 'extends': 'EXTENDS',
    'override': 'OVERRIDE', 'new': 'NEW', 'delete': 'DELETE',
    'try': 'TRY', 'catch': 'CATCH', 'finally': 'FINALLY',
    'namespace': 'NAMESPACE', 'ref': 'REF', 'analyze': 'ANALYZE', 'move': 'MOVE',
    'copy': 'COPY', 'operator': 'OPERATOR', 'macro': 'MACRO', 'const': 'CONST',
    'static': 'STATIC', 'defer': 'DEFER', 'unsafe': 'UNSAFE', 'export': 'EXPORT',
    'var': 'VAR'
}

tokens = [
    'NUMBER', 'STRING', 'ID', 'ASSIGN', 'SEMI', 'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'MOD',
    'ARM', 'POINTER', 'REFERENCE', 'LTHEN', 'RTHEN', 'LBRACKET', 'RBRACKET',
    'LEQ', 'GEQ', 'NEQ', 'EQ', 'OR', 'AND', 'AT',
    'LBRACE', 'RBRACE', 'LPAREN', 'RPAREN', 'LANGLE', 'RANGLE', 'RANGE', 'RANGE_INCLUSIVE',
    'COMMA', 'ARROW'
] + list(reserved.values())

symbol_table = {}

def add_to_symbol_table(name, obj_type):
    symbol_table[name] = obj_type

def lookup_symbol_table(name):
    return symbol_table.get(name, None)


# Tokens rules
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_ASSIGN = r'='
t_SEMI = r';'
t_LANGLE = r'<'
t_RANGLE = r'>'
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_COMMA = r','
t_ARROW = r'->'
t_AND = r'&&'
t_OR = r'\|\|'
t_LTHEN = r'<'
t_RTHEN = r'>'
t_LEQ = r'<='
t_GEQ = r'>='
t_NEQ = r'!='
t_EQ = r'=='
t_ARM = r'=>'
t_AT = r'@'
t_REF = r'&'
t_POINTER = r'\*'
t_RANGE = r'..'
t_RANGE_INCLUSIVE = r'..='

def t_STRING(t):
    r'"[^"]*"'
    t.value = t.value[1:-1]  # Strip the surrounding quotes
    return t

def t_NUMBER(t):
    r'\d+(\.\d+)?'
    t.value = float(t.value) if '.' in t.value else int(t.value)
    return t

def t_COMMENT(t):
    r'//.*'
    pass  # No return value. Token discarded

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'ID')    # Check for reserved words
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

t_ignore = ' \t'

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

lexer = lex.lex()

# Parsing rules

def p_program(p):
    '''program : program statement
               | statement'''
    if len(p) == 3:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = [p[1]]

def p_statement(p):
    '''statement : function_definition
                 | class_definition
                 | struct_definition
                 | enum_definition
                 | namespace_definition
                 | variable_declaration SEMI
                 | interface_definition
                 | return_statement SEMI'''
    p[0] = p[1]

def p_namespace_definition(p):
    '''namespace_definition : NAMESPACE ID LBRACE program RBRACE
                            | NAMESPACE ID LBRACE RBRACE'''
    p[0] = ('namespace_def', p[2], p[4])

def p_return_statement(p):
    '''return_statement : RETURN expression'''
    p[0] = ('return_stmt', p[2])


def p_generics_opt(p):
    '''generics_opt : LANGLE generics_list RANGLE
                    | empty'''
    if len(p) == 4:
        p[0] = ('generics', p[2])  # Capture the list of generic parameters
    else:
        p[0] = []  # No generic parameters

def p_generics_list(p):
    '''generics_list : generics_list COMMA ID
                     | ID'''
    if len(p) == 4:
        p[0] = p[1] + [p[3]]  # Append the ID to the list of generics
    else:
        p[0] = [p[1]]  # Start a new list with the single ID

# Class body with variables and functions
def p_class_body(p):
    '''class_body : class_body class_member
                  | empty'''
    if len(p) == 3:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = []

def p_class_member(p):
    '''class_member : variable_declaration SEMI
                    | function_definition'''
    p[0] = p[1]


# Similarly for struct body
def p_struct_body(p):
    '''struct_body : struct_body struct_member
                   | empty'''
    if len(p) == 3:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = []

def p_struct_member(p):
    '''struct_member : variable_declaration SEMI
                     | function_definition'''
    p[0] = p[1]

def p_enum_body(p):
    '''enum_body : enum_body enum_member
                 | enum_member'''
    if len(p) == 3:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = [p[1]]

def p_enum_member(p):
    '''enum_member : ID
                   | ID ASSIGN NUMBER'''
    # Assign can be used for explicitly specifying the enum value (common in C-like languages)
    if len(p) == 2:
        p[0] = ('enum_variant', p[1])
    else:
        p[0] = ('enum_variant_val', p[1], p[3])

def p_extends_opt(p):
    '''extends_opt : EXTENDS ID generics_opt
                   | empty'''
    if len(p) == 4:
        p[0] = ('extends', p[2], p[3])  # Capture the extended class ID and generics
    else:
        p[0] = None  # No extension

def p_type_specifier(p):
    '''type_specifier : ID
                      | POINTER type_specifier
                      | REF type_specifier'''
    if len(p) == 2:
        # Just an ID, simple type specifier
        p[0] = ('type', p[1])
    else:
        # Handling pointers and references, which have a second component
        p[0] = (p[1] + '_type', p[2])  # Prepend 'POINTER_' or 'REF_' to the type specifier for clarity

def p_expression_boolean(p):
    '''expression : expression AND expression
                  | expression OR expression
                  | expression EQ expression
                  | expression NEQ expression
                  | expression LTHEN expression
                  | expression RTHEN expression
                  | expression LEQ expression
                  | expression GEQ expression'''
    p[0] = ('bool_expr', p[2], p[1], p[3])

def p_for_loop(p):
    '''statement : FOR ID IN expression RANGE expression LBRACE program RBRACE'''
    p[0] = ('for_loop', p[2], p[4], p[6], p[8])

def p_expression_arith(p):
    '''expression : expression PLUS expression
                  | expression MINUS expression
                  | expression TIMES expression
                  | expression DIVIDE expression
                  | expression MOD expression'''
    p[0] = ('arith_expr', p[2], p[1], p[3])

def p_expression_paren(p):
    '''expression : LPAREN expression RPAREN'''
    p[0] = p[2]

# Function calls with parameters
def p_function_call(p):
    '''expression : ID LPAREN arg_list_opt RPAREN'''
    p[0] = ('function_call', p[1], p[3])

def p_arg_list_opt(p):
    '''arg_list_opt : arg_list
                    | empty'''
    p[0] = p[1]

def p_arg_list(p):
    '''arg_list : arg_list COMMA expression
                | expression
                | empty'''
    if len(p) == 4:
        p[0] = p[1] + [p[3]]
    elif len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = []

# Conditional and loop statements
def p_conditional_statement(p):
    '''statement : IF LPAREN expression RPAREN LBRACE program RBRACE
                 | IF LPAREN expression RPAREN LBRACE program RBRACE ELSE LBRACE program RBRACE'''
    if len(p) == 8:
        p[0] = ('if_stmt', p[3], p[6])
    else:
        p[0] = ('if_else_stmt', p[3], p[6], p[10])

def p_while_loop(p):
    '''statement : WHILE LPAREN expression RPAREN LBRACE program RBRACE'''
    p[0] = ('while_loop', p[3], p[6])

# Handling empty productions
def p_empty(p):
    'empty :'
    p[0] = None


def p_expression(p):
    '''expression : NUMBER
                  | ID'''
    p[0] = p[1]

def p_expression_reference(p):
    '''expression : REF ID'''
    p[0] = ('reference_of', p[2])

def p_expression_dereference(p):
    '''expression : POINTER expression'''
    p[0] = ('dereference', p[2])

def p_parameters_opt(p):
    '''parameters_opt : parameters_list
                      | empty'''
    p[0] = p[1]

def p_parameters_list(p):
    '''parameters_list : parameters_list COMMA parameter
                       | parameter'''
    if len(p) == 4:
        p[0] = p[1] + [p[3]]
    else:
        p[0] = [p[1]]

def p_parameter(p):
    '''parameter : type_specifier ID'''
    p[0] = ('param', p[1], p[2])

def p_return_type_opt(p):
    '''return_type_opt : ARROW type_specifier
                       | empty'''
    if len(p) == 3:
        p[0] = ('return_type', p[2])
    else:
        p[0] = None

def p_function_definition(p):
    '''function_definition : FN generics_opt ID LPAREN parameters_opt RPAREN return_type_opt LBRACE program RBRACE'''
    '''                    | FN ID LPAREN parameters_opt RPAREN return_type_opt LBRACE RBRACE'''
    p[0] = ('function_def', p[2], p[3], p[4],p[5], p[6],p[7], p[9])

def p_class_definition(p):
    '''class_definition : CLASS ID generics_opt extends_opt LBRACE class_body RBRACE'''
    if p[4] is not None and p[4][2] is not None:
        extended_class_generics = p[4][2]
        p[6] = [(member[0], member[1], member[2] if len(member) > 2 else None) for member in p[6]]
    else:
        pass
    add_to_symbol_table(p[2], 'class')
    p[0] = ('class_def', p[2], p[3], p[4], p[6])

def p_statement_error(p):
    '''statement : variable_declaration '''
    print(f"Error: missing semicolon at line {p.lineno(1)}")


def p_expression_error(p):
    'expression : error'
    if p[1]:
        print(f"Syntax error in expression at line {p.lineno(1)} near '{p[1].value}'")
    else:
        print("Syntax error in expression with unknown token")
    p[0] = ('error',)


def p_variable_declaration(p):
    '''variable_declaration : type_specifier ID LPAREN arg_list_opt RPAREN SEMI
                            | type_specifier ID SEMI
                            | type_specifier ID ASSIGN type_specifier LPAREN arg_list_opt RPAREN SEMI
                            | type_specifier ID LBRACE arg_list_opt RBRACE SEMI
                            | type_specifier ID ASSIGN NEW type_specifier LPAREN arg_list_opt RPAREN SEMI'''
    obj_type = lookup_symbol_table(p[1])
    if len(p) == 10 and obj_type in ['class', 'struct', 'enum']:
        p[0] = ('dynamic_object_instantiation', obj_type, p[1], p[2])
    elif len(p) == 10:
        p[0] = ('dynamic_variable_instantiation', obj_type, p[1], p[2])
    elif obj_type in ['class', 'struct', 'enum']:
        p[0] = ('object_instantiation', p[1], p[2])
    else:
        p[0] = ('variable_instantiation', p[1], p[2])

def p_interface_definition(p):
    '''interface_definition : INTERFACE ID generics_opt LBRACE interface_body RBRACE'''
    p[0] = ('interface_def', p[2], p[3], p[5])

def p_interface_body(p):
    '''interface_body : interface_body function_definition
                      | empty'''
    if len(p) == 3:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = []

def p_struct_definition(p):
    '''struct_definition : CLASS ID generics_opt extends_opt LBRACE struct_body RBRACE'''
    if p[4] is not None and p[4][2] is not None:
        extended_class_generics = p[4][2]
        p[6] = [(member[0], member[1], member[2] if len(member) > 2 else None) for member in p[6]]
    else:
        pass
    add_to_symbol_table(p[2], 'class')
    p[0] = ('struct_def', p[2], p[3], p[4], p[6])

def p_enum_definition(p):
    """enum_definition : ENUM ID generics_opt LBRACE enum_body RBRACE"""
    add_to_symbol_table(p[2], 'enum')
    p[0] = ('enum_def', p[2], p[4])


def p_error(p):
    if not p:
        print("Syntax error at EOF")
        return

    print(f"Syntax error at token ('{p.value}') at line {p.lineno}")

    # Attempt to recover by finding the next semicolon
    while True:
        tok = yacc.token()  # Get the next token
        if not tok or tok.type == 'SEMI':  # Stop at a semicolon or EOF
            break
    yacc.errok()  # Reset the error flag to continue parsing

parser = yacc.yacc(debug=True)

# Testing the parser
test_data = '''
namespace my_namespace 
{
    fn main() {
        
    }
}
'''


def print_ast(node, level=0):
    indent = "    " * level  # Define indentation (4 spaces per level)

    if node is None:
        print(f"{indent}None")
    elif isinstance(node, tuple):
        # Special handling for certain types of nodes (like variables)
        if node[0] in ['variable_instantiation', 'dynamic_variable_instantiation',
                       'dynamic_object_instantiation', 'object_instantiation']:
            # Print variable instantiation with type and name
            print(f"{indent}{node[0]} - Type: {node[1]}, Name: {node[2]}")
            if len(node) > 3:
                # If there are additional details (e.g., constructor arguments), print them
                print(f"{indent}Arguments:")
                for arg in node[3:]:
                    print_ast(arg, level + 2)
        else:
            # Print the type of the node (first element of the tuple) with indentation
            print(f"{indent}{node[0]}:")

            # Recursively print each element in the tuple starting from the second element
            for subnode in node[1:]:
                if subnode not in ['{', '}']:  # Ignore braces
                    print_ast(subnode, level + 1)
    elif isinstance(node, list):
        # Print each element in the list as a separate node
        for i, subnode in enumerate(node):
            print_ast(subnode, level)
    else:
        # Print the value of the node if it is not a tuple or list
        print(f"{indent}{node}")


# Usage example
ast = parser.parse(test_data)  # Assume this is your AST from the parser
print_ast(ast)