# ****************************************
# IMPORTS
# ****************************************

from strings_with_arrows import *
import string

# ****************************************
# CONSTANTS
# ****************************************

DIGITS = '0123456789'
LETTERS = string.ascii_letters
LETTERS_DIGITS = LETTERS + DIGITS

# ****************************************
# ERRORS
# ****************************************


class Error:
    def __init__(self, pos_start, pos_end, error_name, details):
        self.pos_start = pos_start
        self.pos_end = pos_end
        self.error_name = error_name
        self.details = details

    def as_string(self):
        result = f'{self.error_name}: {self.details}\n'
        result += f'File {self.pos_start.fn}, line {self.pos_start.ln + 1}'
        result += '\n\n' + \
            string_with_arrows(self.pos_start.ftxt,
                               self.pos_start, self.pos_end)
        return result


class IllegalCharError(Error):
    def __init__(self, pos_start, pos_end, details):
        super().__init__(pos_start, pos_end, 'Baka... Illegal character \nNote: ', details)


class ExpectedCharError(Error):
    def __init__(self, pos_start, pos_end, details):
        super().__init__(pos_start, pos_end, 'Baka... Expected character \nNote: ', details)


class InvalidSyntaxError(Error):
    def __init__(self, pos_start, pos_end, details=''):
        super().__init__(pos_start, pos_end, 'Baka... Invalid Syntax \nNote ', details)


class RTError(Error):
    def __init__(self, pos_start, pos_end, details, context):
        super().__init__(pos_start, pos_end, 'Baka... Runtime Error', details)
        self.context = context

    def as_string(self):
        result = self.generate_traceback()
        result += f'{self.error_name}: {self.details}'
        result += '\n\n' + \
            string_with_arrows(self.pos_start.ftxt,
                               self.pos_start, self.pos_end)
        return result

    def generate_traceback(self):
        result = ''
        pos = self.pos_start
        ctx = self.context

        while ctx:
            result = f'  File {pos.fn}, line {str(pos.ln + 1)}, in {ctx.display_name}\n' + result
            pos = ctx.parent_entry_pos
            ctx = ctx.parent

        return 'Traceback (most recent call last):\n' + result

# ****************************************
# POSITION
# ****************************************


class Position:
    def __init__(self, idx, ln, col, fn, ftxt):
        self.idx = idx
        self.ln = ln
        self.col = col
        self.fn = fn
        self.ftxt = ftxt

    def advance(self, curr_char=None):
        self.idx += 1
        self.col += 1

        if curr_char == '\n':
            self.ln += 1
            self.col = 0

        return self

    def copy(self):
        return Position(self.idx, self.ln, self.col, self.fn, self.ftxt)

# ****************************************
# TOKENS
# ****************************************


MAI_INT = 'INT'
MAI_FLOAT = 'FLOAT'
MAI_ADD = 'ADD'
MAI_SUB = 'SUB'
MAI_MULT = 'MULT'
MAI_DIV = 'DIV'
MAI_LPAREN = 'LPAREN'
MAI_RPAREN = 'RPAREN'
MAI_EOF = 'EOF'
MAI_POW = 'POW'
MAI_IDENTIFIER = 'IDENTIFIER'
MAI_KEYWORD = 'KEYWORD'
MAI_EQ = 'EQ'
MAI_EE = 'EE'
MAI_NE = 'NE'
MAI_LT = 'LT'
MAI_GT = 'GT'
MAI_LTE = 'LTE'
MAI_GTE = 'GTE'

KEYWORDS = [
    'MAI',
    'AND',
    'OR',
    'NOT',
]


class Token:
    def __init__(self, type_, value=None, pos_start=None, pos_end=None):
        self.type = type_
        self.value = value

        if pos_start:
            self.pos_start = pos_start.copy()
            self.pos_end = pos_start.copy()
            self.pos_end.advance()

        if pos_end:
            self.pos_end = pos_end.copy()

    def matches(self, type_, value):
        return self.type == type_ and self.value == value

    def __repr__(self):
        if self.value:
            return f'{self.type}:{self.value}'
        return f'{self.type}'

# ****************************************
# LEXER
# ****************************************


class Lexer:
    def __init__(self, fn, text):
        self.fn = fn
        self.text = text
        self.pos = Position(-1, 0, -1, fn, text)
        self.curr_char = None
        self.advance()

    def advance(self):
        self.pos.advance(self.curr_char)
        self.curr_char = self.text[self.pos.idx] if self.pos.idx < len(
            self.text) else None

    def make_tokens(self):
        token = []

        while self.curr_char != None:
            if self.curr_char in ' \t':
                self.advance()
            elif self.curr_char in DIGITS:
                token.append(self.make_number())
            elif self.curr_char in LETTERS:
                token.append(self.make_identifier())
            elif self.curr_char == '+':
                token.append(Token(MAI_ADD, pos_start=self.pos))
                self.advance()
            elif self.curr_char == '-':
                token.append(Token(MAI_SUB, pos_start=self.pos))
                self.advance()
            elif self.curr_char == '*':
                token.append(Token(MAI_MULT, pos_start=self.pos))
                self.advance()
            elif self.curr_char == '/':
                token.append(Token(MAI_DIV, pos_start=self.pos))
                self.advance()
            elif self.curr_char == '^':
                token.append(Token(MAI_POW, pos_start=self.pos))
                self.advance()
            elif self.curr_char == '(':
                token.append(Token(MAI_LPAREN, pos_start=self.pos))
                self.advance()
            elif self.curr_char == ')':
                token.append(Token(MAI_RPAREN, pos_start=self.pos))
                self.advance()
            elif self.curr_char == '!':
                token1, error = self.make_not_equals()
                if error:
                    return [], error
                token.append(token1)
            elif self.curr_char == '=':
                token.append(self.make_equals())
            elif self.curr_char == '<':
                token.append(self.make_less_than())
            elif self.curr_char == '>':
                token.append(self.make_greater_than())
            else:
                pos_start = self.pos.copy()
                char = self.curr_char
                self.advance()
                return [], IllegalCharError(pos_start, self.pos, "'" + char + "'")

        token.append(Token(MAI_EOF, pos_start=self.pos))
        return token, None

    def make_number(self):
        num_str = ''
        dot_count = 0
        pos_start = self.pos.copy()

        while self.curr_char != None and self.curr_char in DIGITS + '.':
            if self.curr_char == '.':
                if dot_count == 1:
                    break
                dot_count += 1
            num_str += self.curr_char
            self.advance()

        if dot_count == 0:
            return Token(MAI_INT, int(num_str), pos_start, self.pos)
        else:
            return Token(MAI_FLOAT, float(num_str), pos_start, self.pos)

    def make_identifier(self):
        id_str = ''
        pos_start = self.pos.copy()

        while self.curr_char != None and self.curr_char in LETTERS_DIGITS + '_':
            id_str += self.curr_char
            self.advance()

        tok_type = MAI_KEYWORD if id_str in KEYWORDS else MAI_IDENTIFIER
        return Token(tok_type, id_str, pos_start, self.pos)

    def make_not_equals(self):
        pos_start = self.pos.copy()
        self.advance()

        if self.curr_char == '=':
            self.advance()
            return Token(MAI_NE, pos_start=pos_start, pos_end=self.pos), None

        self.advance()
        return None, ExpectedCharError(pos_start, self.pos, "'=' (after '!')")

    def make_equals(self):
        tok_type = MAI_EQ
        pos_start = self.pos.copy()
        self.advance()

        if self.curr_char == '=':
            self.advance()
            tok_type = MAI_EE

        return Token(tok_type, pos_start=pos_start, pos_end=self.pos)

    def make_less_than(self):
        tok_type = MAI_LT
        pos_start = self.pos.copy()
        self.advance()

        if self.curr_char == '=':
            self.advance()
            tok_type = MAI_LTE

        return Token(tok_type, pos_start=pos_start, pos_end=self.pos)

    def make_greater_than(self):
        tok_type = MAI_GT
        pos_start = self.pos.copy()
        self.advance()

        if self.curr_char == '=':
            self.advance()
            tok_type = MAI_GTE

        return Token(tok_type, pos_start=pos_start, pos_end=self.pos)

# ****************************************
# NODES
# ****************************************


class NumberNode:
    def __init__(self, tok):
        self.tok = tok

        self.pos_start = self.tok.pos_start
        self.pos_end = self.tok.pos_end

    def __repr__(self):
        return f'{self.tok}'


class VarAccessNode:
    def __init__(self, var_name_tok):
        self.var_name_tok = var_name_tok

        self.pos_start = self.var_name_tok.pos_start
        self.pos_end = self.var_name_tok.pos_end


class VarAssignNode:
    def __init__(self, var_name_tok, value_node):
        self.var_name_tok = var_name_tok
        self.value_node = value_node

        self.pos_start = self.var_name_tok.pos_start
        self.pos_end = self.value_node.pos_end


class BinOpNode:
    def __init__(self, left_node, op_tok, right_node):
        self.left_node = left_node
        self.op_tok = op_tok
        self.right_node = right_node

        self.pos_start = self.left_node.pos_start
        self.pos_end = self.right_node.pos_end

    def __repr__(self):
        return f'({self.left_node}, {self.op_tok}, {self.right_node})'


class UnaryOpNode:
    def __init__(self, op_tok, node):
        self.op_tok = op_tok
        self.node = node

        self.pos_start = self.op_tok.pos_start
        self.pos_end = node.pos_end

    def __repr__(self):
        return f'({self.op_tok}, {self.node})'

# ****************************************
# PARSE RESULT
# ****************************************


class ParseResult:
    def __init__(self):
        self.error = None
        self.node = None
        self.advance_count = 0

    def register_advance(self):
        self.advance_count += 1

    def register(self, res):
        self.advance_count += res.advance_count
        if res.error:
            self.error = res.error
        return res.node

    def success(self, node):
        self.node = node
        return self

    def failure(self, error):
        if not self.error or self.advance_count == 0:
            self.error = error
        return self

# ****************************************
# PARSER
# ****************************************


class Parser:
    def __init__(self, token):
        self.token = token
        self.tok_idx = -1
        self.advance()

    def advance(self, ):
        self.tok_idx += 1
        if self.tok_idx < len(self.token):
            self.curr_tok = self.token[self.tok_idx]
        return self.curr_tok

    def parse(self):
        res = self.expr()
        if not res.error and self.curr_tok.type != MAI_EOF:
            return res.failure(InvalidSyntaxError(
                self.curr_tok.pos_start, self.curr_tok.pos_end,
                "Expected '+', '-', '*', '/', '^', '==', '!=', '<', '>', <=', '>=', 'AND' or 'OR'"
            ))
        return res

    def atom(self):
        res = ParseResult()
        tok = self.curr_tok

        if tok.type in (MAI_INT, MAI_FLOAT):
            res.register_advance()
            self.advance()
            return res.success(NumberNode(tok))

        elif tok.type == MAI_IDENTIFIER:
            res.register_advance()
            self.advance()
            return res.success(VarAccessNode(tok))

        elif tok.type == MAI_LPAREN:
            res.register_advance()
            self.advance()
            expr = res.register(self.expr())
            if res.error:
                return res
            if self.curr_tok.type == MAI_RPAREN:
                res.register_advance()
                self.advance()
                return res.success(expr)
            else:
                return res.failure(InvalidSyntaxError(
                    self.curr_tok.pos_start, self.curr_tok.pos_end,
                    "Expected ')'"
                ))

        return res.failure(InvalidSyntaxError(
            tok.pos_start, tok.pos_end,
            "Expected int, float, identifier, '+', '-', '('"
        ))

    def power(self):
        return self.bin_op(self.atom, (MAI_POW, ), self.factor)

    def factor(self):
        res = ParseResult()
        tok = self.curr_tok

        if tok.type in (MAI_ADD, MAI_SUB):
            res.register_advance()
            self.advance()
            factor = res.register(self.factor())
            if res.error:
                return res
            return res.success(UnaryOpNode(tok, factor))

        return self.power()

    def term(self):
        return self.bin_op(self.factor, (MAI_MULT, MAI_DIV))

    def arith_expr(self):
        return self.bin_op(self.term, (MAI_ADD, MAI_SUB))

    def comp_expr(self):
        res = ParseResult()

        if self.curr_tok.matches(MAI_KEYWORD, 'NOT'):
            op_tok = self.curr_tok
            res.register_advance()
            self.advance()

            node = res.register(self.comp_expr())
            if res.error:
                return res
            return res.success(UnaryOpNode(op_tok, node))

        node = res.register(self.bin_op(
            self.arith_expr, (MAI_EE, MAI_NE, MAI_LT, MAI_GT, MAI_LTE, MAI_GTE)))

        if res.error:
            return res.failure(InvalidSyntaxError(
                self.curr_tok.pos_start, self.curr_tok.pos_end,
                "Expected int, float, identifier, '+', '-', '(' or 'NOT'"
            ))

        return res.success(node)

    def expr(self):
        res = ParseResult()

        if self.curr_tok.matches(MAI_KEYWORD, 'MAI'):
            res.register_advance()
            self.advance()

            if self.curr_tok.type != MAI_IDENTIFIER:
                return res.failure(InvalidSyntaxError(
                    self.curr_tok.pos_start, self.curr_tok.pos_end,
                    "Expected identifier"
                ))

            var_name = self.curr_tok
            res.register_advance()
            self.advance()

            if self.curr_tok.type != MAI_EQ:
                return res.failure(InvalidSyntaxError(
                    self.curr_tok.pos_start, self.curr_tok.pos_end,
                    "Expected '='"
                ))

            res.register_advance()
            self.advance()
            expr = res.register(self.expr())
            if res.error:
                return res
            return res.success(VarAssignNode(var_name, expr))

        node = res.register(self.bin_op(
            self.comp_expr, ((MAI_KEYWORD, 'AND'), (MAI_KEYWORD, 'OR'))))

        if res.error:
            return res.failure(InvalidSyntaxError(
                self.curr_tok.pos_start, self.curr_tok.pos_end,
                "Expected 'MAI', int, float, identifier, '+', '-', '(' or 'NOT'"
            ))

        return res.success(node)

    def bin_op(self, func_a, ops, func_b=None):
        if func_b == None:
            func_b = func_a

        res = ParseResult()
        left = res.register(func_a())
        if res.error:
            return res

        while self.curr_tok.type in ops or (self.curr_tok.type, self.curr_tok.value) in ops:
            op_tok = self.curr_tok
            res.register_advance()
            self.advance()
            right = res.register(func_b())
            if res.error:
                return res
            left = BinOpNode(left, op_tok, right)

        return res.success(left)

# ****************************************
# RUNTIME RESULT
# ****************************************


class RTResult:
    def __init__(self):
        self.value = None
        self.error = None

    def register(self, res):
        if res.error:
            self.error = res.error
        return res.value

    def success(self, value):
        self.value = value
        return self

    def failure(self, error):
        self.error = error
        return self

# ****************************************
# VALUES
# ****************************************


class Number:
    def __init__(self, value):
        self.value = value
        self.set_pos()
        self.set_context()

    def set_pos(self, pos_start=None, pos_end=None):
        self.pos_start = pos_start
        self.pos_end = pos_end
        return self

    def set_context(self, context=None):
        self.context = context
        return self

    def added_to(self, other):
        if isinstance(other, Number):
            return Number(self.value + other.value).set_context(self.context), None

    def subtract_by(self, other):
        if isinstance(other, Number):
            return Number(self.value - other.value).set_context(self.context), None

    def multiply_by(self, other):
        if isinstance(other, Number):
            return Number(self.value * other.value).set_context(self.context), None

    def divided_by(self, other):
        if isinstance(other, Number):
            if other.value == 0:
                return None, RTError(
                    other.pos_start, other.pos_end,
                    "Don't divide a number with ZERO",
                    self.context
                )

            return Number(self.value / other.value).set_context(self.context), None

    def power_by(self, other):
        if isinstance(other, Number):
            return Number(self.value ** other.value).set_context(self.context), None

    def get_comparison_eq(self, other):
        if isinstance(other, Number):
            return Number(int(self.value == other.value)).set_context(self.context), None

    def get_comparison_ne(self, other):
        if isinstance(other, Number):
            return Number(int(self.value != other.value)).set_context(self.context), None

    def get_comparison_lt(self, other):
        if isinstance(other, Number):
            return Number(int(self.value < other.value)).set_context(self.context), None

    def get_comparison_gt(self, other):
        if isinstance(other, Number):
            return Number(int(self.value > other.value)).set_context(self.context), None

    def get_comparison_lte(self, other):
        if isinstance(other, Number):
            return Number(int(self.value <= other.value)).set_context(self.context), None

    def get_comparison_gte(self, other):
        if isinstance(other, Number):
            return Number(int(self.value >= other.value)).set_context(self.context), None

    def anded_by(self, other):
        if isinstance(other, Number):
            return Number(int(self.value and other.value)).set_context(self.context), None

    def ored_by(self, other):
        if isinstance(other, Number):
            return Number(int(self.value or other.value)).set_context(self.context), None

    def notted(self):
        return Number(1 if self.value == 0 else 0).set_context(self.context), None

    def copy(self):
        copy = Number(self.value)
        copy.set_pos(self.pos_start, self.pos_end)
        copy.set_context(self.context)
        return copy

    def __repr__(self):
        return str(self.value)

# ****************************************
# CONTEXT
# ****************************************


class Context:
    def __init__(self, display_name, parent=None, parent_entry_pos=None):
        self.display_name = display_name
        self.parent = parent
        self.parent_entry_pos = parent_entry_pos
        self.symbol_table = None

# ****************************************
# SYMBOL TABLE
# ****************************************


class SymbolTable:
    def __init__(self):
        self.symbols = {}
        self.parent = None

    def get(self, name):
        value = self.symbols.get(name, None)
        if value == None and self.parent:
            return self.parent.get(name)
        return value

    def set(self, name, value):
        self.symbols[name] = value

    def remove(self, name):
        del self.symbols[name]

# ****************************************
# INTERPRETER
# ****************************************


class Interpreter:
    def visit(self, node, context):
        method_name = f'visit_{type(node).__name__}'
        method = getattr(self, method_name, self.no_visit_method)
        return method(node, context)

    def no_visit_method(self, node, context):
        raise Exception(f'No visit_{type(node).__name__} method defined')

    ###################################

    def visit_NumberNode(self, node, context):
        return RTResult().success(
            Number(node.tok.value).set_context(
                context).set_pos(node.pos_start, node.pos_end)
        )

    def visit_VarAccessNode(self, node, context):
        res = RTResult()
        var_name = node.var_name_tok.value
        value = context.symbol_table.get(var_name)

        if not value:
            return res.failure(RTError(
                node.pos_start, node.pos_end,
                f"'{var_name}' is not defined",
                context
            ))

        value = value.copy().set_pos(node.pos_start, node.pos_end)
        return res.success(value)

    def visit_VarAssignNode(self, node, context):
        res = RTResult()
        var_name = node.var_name_tok.value
        value = res.register(self.visit(node.value_node, context))
        if res.error:
            return res

        context.symbol_table.set(var_name, value)
        return res.success(value)

    def visit_BinOpNode(self, node, context):
        res = RTResult()
        left = res.register(self.visit(node.left_node, context))
        if res.error:
            return res
        right = res.register(self.visit(node.right_node, context))
        if res.error:
            return res

        if node.op_tok.type == MAI_ADD:
            result, error = left.added_to(right)
        elif node.op_tok.type == MAI_SUB:
            result, error = left.subtract_by(right)
        elif node.op_tok.type == MAI_MULT:
            result, error = left.multiply_by(right)
        elif node.op_tok.type == MAI_DIV:
            result, error = left.divided_by(right)
        elif node.op_tok.type == MAI_POW:
            result, error = left.power_by(right)
        elif node.op_tok.type == MAI_EE:
            result, error = left.get_comparison_eq(right)
        elif node.op_tok.type == MAI_NE:
            result, error = left.get_comparison_ne(right)
        elif node.op_tok.type == MAI_LT:
            result, error = left.get_comparison_lt(right)
        elif node.op_tok.type == MAI_GT:
            result, error = left.get_comparison_gt(right)
        elif node.op_tok.type == MAI_LTE:
            result, error = left.get_comparison_lte(right)
        elif node.op_tok.type == MAI_GTE:
            result, error = left.get_comparison_gte(right)
        elif node.op_tok.matches(MAI_KEYWORD, 'AND'):
            result, error = left.anded_by(right)
        elif node.op_tok.matches(MAI_KEYWORD, 'OR'):
            result, error = left.ored_by(right)

        if error:
            return res.failure(error)
        else:
            return res.success(result.set_pos(node.pos_start, node.pos_end))

    def visit_UnaryOpNode(self, node, context):
        res = RTResult()
        number = res.register(self.visit(node.node, context))
        if res.error:
            return res

        error = None

        if node.op_tok.type == MAI_SUB:
            number, error = number.multiply_by(Number(-1))
        elif node.op_tok.matches(MAI_KEYWORD, 'NOT'):
            number, error = number.notted()

        if error:
            return res.failure(error)
        else:
            return res.success(number.set_pos(node.pos_start, node.pos_end))

# ****************************************
# RUN
# ****************************************


global_symbol_table = SymbolTable()
global_symbol_table.set("NULL", Number(0))
global_symbol_table.set("FALSE", Number(0))
global_symbol_table.set("TRUE", Number(1))


def run(fn, text):
    # Generate token
    lexer = Lexer(fn, text)
    token, error = lexer.make_tokens()
    if error:
        return None, error

    # Generate AST
    parser = Parser(token)
    ast = parser.parse()
    if ast.error:
        return None, ast.error

    # Run program
    interpreter = Interpreter()
    context = Context('<program>')
    context.symbol_table = global_symbol_table
    result = interpreter.visit(ast.node, context)

    return result.value, result.error
