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
    'try': 'TRY', 'catch': 'CATCH', 'finally': 'FINALLY', 'i32': 'I32', 'i64': 'I64',
    'f32': 'F32', 'f64': 'F64', 'u32': 'U32', 'u64': 'U64', 'bool': 'BOOL', 'void': 'VOID',
    'var': 'VAR', 'string': 'STRING', 'i8': 'I8', 'i16': 'I16', 'u8': 'U8', 'u16': 'U16', 'type': 'TYPE',
    'namespace': 'NAMESPACE', 'ref': 'REF', 'analyze': 'ANALYZE', 'move': 'MOVE',
    'copy': 'COPY', 'operator': 'OPERATOR', 'macro': 'MACRO', 'const': 'CONST',
    'static': 'STATIC', 'defer': 'DEFER', 'unsafe': 'UNSAFE', 'export': 'EXPORT',
    'async': 'ASYNC', 'await': 'AWAIT'
}

tokens = [
    'NUMBER', 'ID', 'ASSIGN', 'SEMI', 'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'MOD', 'ALL',
    'ARM', 'POINTER', 'REFERENCE', 'LTHEN', 'RTHEN', 'LBRACKET', 'RBRACKET', 'DOUBLE_COLON',
    'LEQ', 'GEQ', 'NEQ', 'EQ', 'OR', 'AND', 'AT', 'NOT', 'COLON',
    'LBRACE', 'RBRACE', 'LPAREN', 'RPAREN', 'LANGLE', 'RANGLE', 'RANGE', 'RANGE_INCLUSIVE',
    'COMMA', 'ARROW', 'UNDERSCORE', 'INCREMENT', 'DECREMENT'
] + list(reserved.values())

precedence = (
    ('left', 'OR'),
    ('left', 'AND'),
    ('left', 'EQ', 'NEQ'),
    ('left', 'LTHEN', 'RTHEN', 'LEQ', 'GEQ'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
)

# Tokens rules
t_DOUBLE_COLON = r'::'
t_COLON = r':'
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
t_UNDERSCORE = r'_'
t_INCREMENT = r'\+\+'
t_DECREMENT = r'--'

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
               | statement
               | empty'''
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
                 | variable SEMI
                 | interface_definition
                 | return_statement SEMI
                 | use_statement'''
    p[0] = p[1]
def p_expression_boolean_literals(p):
    '''expression : TRUE
                  | FALSE'''
    p[0] = ('bool_literal', p[1] == 'true')

def p_namespace_definition(p):
    '''namespace_definition : NAMESPACE ID block'''
    p[0] = ('namespace_def', p[2], p[3])

def p_return_statement(p):
    '''return_statement : RETURN expression'''
    p[0] = ('return_stmt', p[2])

def p_block(p):
    '''block : LBRACE program RBRACE'''
    if len(p) == 4 and p[2] != None:
        p[0] = p[2]
    else:
        p[0] = []

def p_type_with_generics(p):
    '''type_with_generics : ID LANGLE type_list RANGLE
                          | ID empty'''

    if len(p) == 5:
        p[0] = ('type_with_generics', p[1], p[3])
    else:
        p[0] = ('type_with_generics', p[1])

def p_type_list(p):
    '''type_list : type_list PLUS type_specifier
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
    '''class_member : variable SEMI
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

def p_expression_postfix(p):
    '''expression : expression INCREMENT
                  | expression DECREMENT'''
    if p[2] == '++':
        p[0] = ('postfix_increment', p[1])
    else:
        p[0] = ('postfix_decrement', p[1])

def p_expression_prefix(p):
    '''expression : INCREMENT expression
                  | DECREMENT expression'''

    if p[1] == '++':
        p[0] = ('prefix_increment', p[2])
    else:
        p[0] = ('prefix_decrement', p[2])

def p_struct_member(p):
    '''struct_member : variable SEMI
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

def p_generic_param_list(p):
    '''generic_param_list : LANGLE generic_params RANGLE'''
    if len(p) == 4:
        p[0] = ('generic_params', p[2], None)
    else:
        p[0] = ('generic_params', p[2], p[4])

def p_generic_params(p):
    '''generic_params : generic_params COMMA generic_param
                      | generic_param'''
    if len(p) == 4:
        p[0] = p[1] + [p[3]]
    else:
        p[0] = [p[1]]

def p_generic_param(p):
    '''generic_param : ID
                     | type_specifier ID'''
    if len(p) == 2:
        p[0] = ('type_param', p[1])
    else:
        p[0] = ('value_param', p[1], p[2])


def p_type_specifier(p):
    '''type_specifier : ID
                      | I8
                      | I16
                      | I32
                      | I64
                      | U8
                      | U16
                      | U32
                      | U64
                      | F32
                      | F64
                      | BOOL
                      | STRING
                      | VOID
                      | TYPE
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
    p[0] = ('bool_expr', p[2], p[1], p[3])

def p_for_loop(p):
    """
    statement : for_block
    """
    # Extract the expression, which is in the 4th position for the first pattern
    # and in the 5th position for the second pattern (with parentheses)
    p[0] = ('for_loop', p[1])

def p_for_block(p):
    '''
    for_block : FOR ID IN expression block
              | FOR LPAREN ID IN expression RPAREN block
    '''

    id = p[2] if len(p) == 6 else p[3]
    expr = p[5] if len(p) == 8 else p[4]
    block = p[7] if len(p) == 8 else p[5]
    p[0] = ('for_block', id, expr, block)


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
    p[0] = p[1] if p[1] is not None else []

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

def p_if_statement(p):
    '''
    statement : if_block else_if_blocks else_block
              | if_block else_if_blocks
              | if_block else_block
              | if_block
    '''
    if_stmt = ('if_stmt', p[1])
    if len(p) > 2:
        # Append additional items directly to a tuple by concatenation
        if_stmt += tuple(p[2:])
    p[0] = if_stmt

def p_if_block(p):
    '''
    if_block : IF expression block
    '''
    p[0] = ('if_block', p[2], p[3])

def p_else_if_blocks(p):
    '''
    else_if_blocks : else_if_blocks else_if_block
                   | else_if_block
    '''
    # This rule allows for recursion, supporting multiple else-if statements
    if len(p) == 3:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = [p[1]]

def p_else_if_block(p):
    '''
    else_if_block : ELSE IF expression block
    '''
    p[0] = ('else_if_block', p[3], p[4])

def p_else_block(p):
    '''
    else_block : ELSE block
    '''
    p[0] = ('else_block', p[2])

def p_match_statement(p):
    '''statement : MATCH expression LBRACE match_cases RBRACE'''
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
    '''function_definition : ASYNC FN ID LPAREN parameters_opt RPAREN return_type_opt block'''
    p[0] = ('async_function_def', p[3], p[5], p[7], p[8])

def p_expression_await(p):
    '''expression : AWAIT expression'''
    p[0] = ('await_expression', p[2])

def p_while_loop(p):
    '''statement : while_block'''
    p[0] = ('while_loop', p[1])

def p_while_block(p):
    '''while_block : WHILE expression block'''
    p[0] = ('while_block', p[2], p[3])

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
    '''function_definition : FN ID generic_param_list LPAREN parameters_opt RPAREN return_type_opt block
                           | FN ID LPAREN parameters_opt RPAREN return_type_opt block'''
    function_name = p[2]

    # Check if generic parameters are present
    if len(p) == 9:
        generic_params = p[3][1]  # Extract generic params
        constraints = p[3][2]  # Extract constraints from the 'where' clause
        parameters = p[5]
        return_type = p[7]
        function_body = p[8]
        p[0] = (
        'function_definition', function_name, generic_params, constraints, parameters, return_type, function_body)
    else:
        # This branch handles functions without generic parameters
        parameters = p[4]
        return_type = p[6]
        function_body = p[7]
        p[0] = ('function_definition', function_name, None, None, parameters, return_type, function_body)


def p_class_definition(p):
    '''class_definition : CLASS ID generic_param_list extends_opt LBRACE class_body RBRACE'''
    class_name = p[2]
    generic_params = p[3][1]  # First element of the tuple returned by generic_param_list
    constraints = p[3][2]  # Second element (may be None if no 'where' clause)
    extends = p[4]
    class_body = p[6]
    p[0] = ('class_definition', class_name, generic_params, constraints, extends, class_body)


def p_statement_error(p):
    '''statement : variable '''
    p[0] = p[1]

def p_assign_statement(p):
    '''statement : ID ASSIGN expression'''
    p[0] = ('assign', p[1], p[3])

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


def p_variable(p):
    '''
    variable : type_specifier ID
             | type_specifier ID ASSIGN expression
             | type_specifier ID LPAREN arg_list_opt RPAREN
             | type_specifier ID LBRACE arg_list_opt RBRACE
             | type_specifier ID ASSIGN NEW type_specifier LPAREN arg_list_opt RPAREN
             | type_specifier ID ASSIGN expression LPAREN arg_list_opt RPAREN
    '''
    if len(p) == 3:  # Simple variable declaration
        p[0] = ('variable_declaration', p[1], p[2])
    elif len(p) == 5 and p[3] == '=':  # Variable assignment with initializer
        p[0] = ('variable_initialization', p[1], p[2], p[4])
    elif len(p) == 6 and p[3] == '(' and p[5] == ')':  # Function-like declaration
        p[0] = ('variable_func_like_decl', p[1], p[2], p[4])
    elif len(p) == 6 and p[3] == '{' and p[5] == '}':  # Brace initialization
        p[0] = ('variable_brace_init', p[1], p[2], p[4])
    elif len(p) == 10 and p[3] == '=':  # New type specifier with initializer
        p[0] = ('variable_new_type_init', p[1], p[2], p[5], p[7], p[9])
    elif len(p) == 7 and p[3] == '=':  # Assignment with expression and function call
        p[0] = ('variable_expr_func_call', p[1], p[2], p[4], p[6])

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
    '''struct_definition : STRUCT ID generic_param_list extends_opt LBRACE struct_body RBRACE'''
    struct_name = p[2]
    generic_params = p[3][1]  # Extract generic params
    constraints = p[3][2]  # Extract constraints from the 'where' clause
    extends = p[4]
    struct_body = p[6]
    p[0] = ('struct_definition', struct_name, generic_params, constraints, extends, struct_body)


def p_enum_definition(p):
    """enum_definition : ENUM ID generic_param_list LBRACE enum_body RBRACE"""
    enum_name = p[2]
    generic_params = p[3][1]  # Extract generic params
    constraints = p[3][2]  # Extract constraints
    enum_body = p[5]
    p[0] = ('enum_definition', enum_name, generic_params, constraints, enum_body)



def p_error(p):
    if p:
        print(f"Syntax error at token {p.type} ('{p.value}') at line {p.lineno}")
    else:
        print("Syntax error at EOF")

    # Attempt to recover by finding the next semicolon
    while True:
        tok = p.lexer.token()  # Correctly get the next token from the lexer
        if not tok:  # If no more tokens, break out of the loop
            break
        if tok.type == 'SEMI':  # Stop at a semicolon
            break
    yacc.errok()  # Reset the error flag to continue parsing


parser = yacc.yacc(debug=True)

# Testing the parser
test_data = '''

use std::io;

namespace my_namespace 
{
    class my_class <my_class T, i32 U>
    {
        i32 x;
        fn my_function(my_class<i32> x) -> i32 
        {
            
        }
    }
    
    fn main() -> void 
    {
        my_class<T> e;
        
        if (my_function(x) == 0)
        {
            return 0;
        } 
        else if (x >= 5) 
        {
            return 1;
        } 
        else 
        {
            return 2;
        }
        
        for (x in y) 
        {
            return 0;
        }
        
        while (x) 
        {
            return 0;
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

print(ast)