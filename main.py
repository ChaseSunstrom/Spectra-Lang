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
    'var': 'VAR', 'async': 'ASYNC', 'await': 'AWAIT'
}

tokens = [
    'NUMBER', 'STRING', 'ID', 'ASSIGN', 'SEMI', 'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'MOD', 'ALL',
    'ARM', 'POINTER', 'REFERENCE', 'LTHEN', 'RTHEN', 'LBRACKET', 'RBRACKET', 'DOUBLE_COLON',
    'LEQ', 'GEQ', 'NEQ', 'EQ', 'OR', 'AND', 'AT', 'NOT',
    'LBRACE', 'RBRACE', 'LPAREN', 'RPAREN', 'LANGLE', 'RANGLE', 'RANGE', 'RANGE_INCLUSIVE',
    'COMMA', 'ARROW', 'UNDERSCORE'
] + list(reserved.values())

symbol_table = {}

def add_to_symbol_table(name, obj_type):
    symbol_table[name] = obj_type

def lookup_symbol_table(name):
    return symbol_table.get(str(name), None)

precedence = (
    ('left', 'OR'),
    ('left', 'AND'),
    ('left', 'EQ', 'NEQ'),  # Non-associative operators
    ('left', 'LTHEN', 'RTHEN', 'LEQ', 'GEQ'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
)

# Tokens rules
t_DOUBLE_COLON = r'::'
t_NOT = r'!'
t_ALL = r'\*'
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
t_UNDERSCORE = r'_'

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
    print(f"Illegal character {t.value[0]}, at line {t.lexer.lineno}")
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
def p_expression_boolean_literals(p):
    '''expression : TRUE
                  | FALSE'''
    p[0] = ('bool_literal', p[1] == 'true')


def p_namespace_definition(p):
    '''namespace_definition : NAMESPACE ID LBRACE program RBRACE
                            | NAMESPACE ID LBRACE RBRACE'''
    p[0] = ('namespace_def', p[2], p[4])

def p_return_statement(p):
    '''return_statement : RETURN expression'''
    p[0] = ('return_stmt', p[2])

def p_block(p):
    '''block : LBRACE program RBRACE'''
    p[0] = p[2]

def p_type_with_generics(p):
    '''type_with_generics : ID LANGLE type_list RANGLE
                          | ID empty'''

    if len(p) == 5:
        p[0] = ('type_with_generics', p[1], p[3])
    else:
        p[0] = ('type_with_generics', p[1])

def p_type_list(p):
    '''type_list : type_list COMMA type_specifier
                 | type_specifier'''
    if len(p) == 4:
        p[0] = p[1] + [p[3]]
    else:
        p[0] = [p[1]]

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
    '''extends_opt : EXTENDS type_with_generics
                   | empty'''
    if len(p) == 4:
        p[0] = ('extends', p[2], p[3])  # Capture the extended class ID and generics
    else:
        p[0] = None  # No extension

def p_type_specifier(p):
    '''type_specifier : ID
                      | type_with_generics
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
                  | expression GEQ expression
                  | NOT expression'''
    if len(p) == 4:
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

def p_match_statement(p):
    '''statement : MATCH LPAREN expression RPAREN LBRACE match_cases RBRACE'''
    p[0] = ('match_statement', p[3], p[6])

def p_match_cases(p):
    '''match_cases : match_cases match_case
                   | match_case'''
    if len(p) == 3:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = [p[1]]

def p_match_case(p):
    '''match_case : pattern ARM program SEMI'''
    p[0] = ('match_case', p[1], p[3])

def p_pattern(p):
    '''pattern : simple_pattern
               | tuple_pattern'''
    p[0] = p[1]

def p_simple_pattern(p):
    '''simple_pattern : ID
                      | NUMBER
                      | STRING
                      | UNDERSCORE'''
    p[0] = ('simple_pattern', p[1])

def p_tuple_pattern(p):
    '''tuple_pattern : LPAREN pattern_list RPAREN'''
    p[0] = ('tuple_pattern', p[2])

def p_pattern_list(p):
    '''pattern_list : pattern_list COMMA pattern
                    | pattern'''
    if len(p) == 4:
        p[0] = p[1] + [p[3]]
    else:
        p[0] = [p[1]]

def p_qualified_name(p):
    '''qualified_name : qualified_name DOUBLE_COLON ID
                      | ID'''
    if len(p) == 4:
        # Concatenate the qualified name with the next identifier
        p[0] = ('qualified_name', p[1], p[3])
    else:
        # Single identifier
        p[0] = ('qualified_name', p[1])

def p_use_statement(p):
    '''use_statement : USE qualified_name SEMI'''
    p[0] = ('use_module', p[2])


def p_async_function_definition(p):
    '''function_definition : ASYNC FN ID LPAREN parameters_opt RPAREN return_type_opt LBRACE program RBRACE'''
    p[0] = ('async_function_def', p[3], p[5], p[7], p[9])

def p_expression_await(p):
    '''expression : AWAIT expression'''
    p[0] = ('await_expression', p[2])

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
        p[0] = ('return_type', 'void')

def p_function_definition(p):
    '''function_definition : FN type_with_generics LPAREN parameters_opt RPAREN return_type_opt LBRACE program RBRACE
                           | FN type_with_generics LPAREN parameters_opt RPAREN return_type_opt LBRACE RBRACE'''
    function_name = p[2][1]  # Assuming the function name is always present
    # No need for parentheses in the output, they're part of the syntax, not the data structure

    parameters = p[4]  # Directly take the parameters list
    return_type = p[6]  # Capture the return type, which could be None or a specific type

    # For the function body, check if it's empty or contains statements
    function_body = p[8] if len(p) > 8 else None

    p[0] = ('function_def', function_name, parameters, return_type, function_body)


def p_class_definition(p):
    '''class_definition : CLASS type_with_generics extends_opt LBRACE class_body RBRACE'''
    # Assuming type_with_generics could return either ('type_with_generics', ID, [generics])
    # or just (ID,) when there are no generics
    if isinstance(p[2], tuple) and len(p[2]) == 3:
        # When generics are present
        class_name = p[2][1]
        class_generics = p[2][2]  # The generics list
    elif isinstance(p[2], tuple) and len(p[2]) == 2:
        # When there are no generics, and the rule returns a tuple with only ID
        class_name = p[2][1]
        class_generics = None
    else:
        # Fallback in case the structure is different than expected, adjust as necessary
        class_name = p[2]
        class_generics = None

    extends_info = p[3]  # This is either None or a tuple ('extends', base_class_id, [base_class_generics])

    if extends_info is not None and len(extends_info) > 1:
        # Extract base class ID and optionally generics if provided
        base_class_id = extends_info[1]
        base_class_generics = extends_info[2] if len(extends_info) == 3 else None
    else:
        base_class_id = None
        base_class_generics = None

    # Updating the symbol table to include the new class definition
    add_to_symbol_table(class_name, 'class')

    # Construct the tuple for the class definition including all the extracted information
    p[0] = ('class_def', class_name, class_generics, base_class_id, base_class_generics, p[5])


def p_statement_error(p):
    '''statement : variable_declaration '''
    p[0] = p[1]

def p_statement_assign(p):
    'statement : ID ASSIGN expression'
    p[1] = p[3]

def p_statement_expr(p):
    'statement : expression'
    print(p[1])


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
                            | type_specifier ID ASSIGN expression SEMI
                            | type_specifier ID LBRACE arg_list_opt RBRACE SEMI
                            | type_specifier ID ASSIGN NEW type_specifier LPAREN arg_list_opt RPAREN SEMI'''

    if len(p) == 4:  # Simple variable declaration: type_specifier ID SEMI
        p[0] = ('variable_declaration', p[1], p[2])
    elif len(p) == 6 and p[3] == 'ASSIGN':  # Simple assignment: type_specifier ID ASSIGN expression SEMI
        p[0] = ('variable_assignment', p[1], p[2], p[4])
    elif len(p) == 7:  # Object instantiation without assignment: type_specifier ID LBRACE arg_list_opt RBRACE SEMI
        obj_type = lookup_symbol_table(p[1])
        if obj_type in ['class', 'struct', 'enum']:
            p[0] = ('object_instantiation', obj_type, p[1], p[2], p[4])
        else:
            raise SyntaxError(f"Type {p[1]} not a class, struct, or enum for instantiation.")
    elif len(p) == 10:  # Variable declaration with 'new' for dynamic instantiation
        obj_type = lookup_symbol_table(p[1])
        if p[4] == "NEW":  # With 'new': type_specifier ID ASSIGN NEW type_specifier LPAREN arg_list_opt RPAREN SEMI
            p[0] = ('object_dynamic_instantiation', obj_type, p[1], p[2], p[5], p[7])
        else:
            p[0] = ('variable_dynamic_instantiation', p[1], p[2], p[4], p[6])


def p_interface_definition(p):
    '''interface_definition : INTERFACE type_with_generics LBRACE interface_body RBRACE'''
    p[0] = ('interface_def', p[2], p[3], p[5])

def p_interface_body(p):
    '''interface_body : interface_body function_definition
                      | empty'''
    if len(p) == 3:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = []

def p_struct_definition(p):
    '''struct_definition : CLASS type_with_generics extends_opt LBRACE struct_body RBRACE'''
    if p[4] is not None and p[4][2] is not None:
        extended_class_generics = p[4][2]
        p[6] = [(member[0], member[1], member[2] if len(member) > 2 else None) for member in p[6]]
    else:
        pass
    add_to_symbol_table(p[2], 'class')
    p[0] = ('struct_def', p[2], p[3], p[4], p[6])

def p_enum_definition(p):
    """enum_definition : ENUM type_with_generics LBRACE enum_body RBRACE"""
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
use std::io;

namespace my_namespace 
{
    class my_class {
        fn my_function() -> i32 {
            
        }
    }
    
    fn main() -> i32 {
        i32 x = 1;
        match (x) {
            1 => 1;
            2 => 2;
            _ => 3;
        }
    }   
}
'''

def print_ast(node, level=0):
    indent = "    " * level  # Define indentation (4 spaces per level)

    if node is None:
        print(f"{indent}None")
    elif isinstance(node, tuple):
        print(f"{indent}{node[0]}:", end="")
        if len(node) > 1:
            print()  # Move to the next line for additional details
            for subnode in node[1:]:
                print_ast(subnode, level + 1)
        else:
            print(" Empty")  # Indicate that the node has no further details
    elif isinstance(node, list):
        if node:
            for subnode in node:
                print_ast(subnode, level)
    else:
        # Directly print the value of the node if it is not a tuple or list
        print(f"{indent}{node}")

# Usage example
ast = parser.parse(test_data)  # Assume this is your AST from the parser
print_ast(ast)