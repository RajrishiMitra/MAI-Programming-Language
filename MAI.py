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
# ERROR
# ****************************************


class Error:
    def __init__(self, pos_start, pos_end, error_name, details):
        self.pos_start = pos_start
        self.pos_end = pos_end
        self.error_name = error_name
        self.details = details

    def as_string(self):
        result = f'{self.error_name}: {self.details}'
        result += '\n'
        result += f'File {self.pos_start.fn}, Line {self.pos_start.ln + 1}'
        result += f'\n\n' + \
            string_with_arrows(self.pos_start.ftxt,
                               self.pos_start, self.pos_end)
        return result


class IllegalCharError(Error):
    def __init__(self, pos_start, pos_end, details):
        super().__init__(pos_start, pos_end, 'Baka... Illegal character \n Note: ', details)


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
            self.col += 1
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
MAI_LAPREN = 'LPAREN'
MAI_RAPREN = 'RPAREN'
MAI_EOF = 'EOF'
MAI_POW = 'POW'
MAI_IDENTIFIER = 'IDENTIFIER'
MAI_KEYWORD = 'KEYWORD'
MAI_EQ = 'EQ'

KEYWORDS = [
    'MAI'
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

    def __repr__(self):
        if self.value:
            return f"{self.type}:{self.value}"
        return f"{self.type}"

    def matches(self, type_, value):
        return self.type == type_ and self.value == value

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

    def make_token(self):
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
            elif self.curr_char == '=':
                token.append(Token(MAI_EQ, pos_start=self.pos))
                self.advance()
            elif self.curr_char == '(':
                token.append(Token(MAI_LAPREN, pos_start=self.pos))
                self.advance()
            elif self.curr_char == ')':
                token.append(Token(MAI_RAPREN, pos_start=self.pos))
                self.advance()
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
                num_str += '.'
            else:
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
        self.pos_start = self.value_node.pos_start
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
    def __init__(self, tokens):
        self.tokens = tokens
        self.tok_idx = -1
        self.advance()

    def advance(self):
        self.tok_idx += 1
        if self.tok_idx < len(self.tokens):
            self.curr_tok = self.tokens[self.tok_idx]
        return self.curr_tok

    def bin_op(self, func1, ops, func2=None):
        if func2 == None:
            func2 = func1
        res = ParseResult()
        left = res.register(func1())

        if res.error:
            return res

        while self.curr_tok.type in ops:
            op_tok = self.curr_tok
            res.register_advance()
            self.advance()
            right = res.register(func2())

            if res.error:
                return res
            left = BinOpNode(left, op_tok, right)
        return res.success(left)

    def parse(self):
        res = self.expr()
        if not res.error and self.curr_tok.type != MAI_EOF:
            return res.failure(InvalidSyntaxError(
                self.curr_tok.pos_start, self.curr_tok.pos_end,
                "Expected '+' '-' '*' '/'"
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

        elif tok.type == MAI_LAPREN:
            res.register_advance()
            self.advance()
            expr = res.register(self.expr())
            if res.error:
                return res

            if self.curr_tok.type == MAI_RAPREN:
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
            "Expected 'INT', 'FLOAT', 'IDENTIFIER', '+', '-' or '('"
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

    def expr(self):
        res = ParseResult()
        if self.curr_tok.matches(MAI_KEYWORD, 'MAI'):
            res.register_advance()
            self.advance()

            if self.curr_tok.type != MAI_IDENTIFIER:
                return res.failure(InvalidSyntaxError(
                    self.curr_tok.pos_start, self.curr_tok.pos_end,
                    "Expected IDENTIFIER"
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

        node = res.register(self.bin_op(self.term, (MAI_ADD, MAI_SUB)))

        if res.error:
            return res.failure(InvalidSyntaxError(
                self.curr_tok.pos_start, self.curr_tok.pos_end,
                "Expected 'MAI', INT, FLOAT, IDENTIFIER, '+', '-' or '('"
            ))
        return res.success(node)

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
        raise Exception(f'No visit {type(node).__name__} method defined')

    def visit_NumberNode(self, node, context):
        return RTResult().success(Number(node.tok.value).set_context(context).set_pos(node.pos_start, node.pos_end))

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
            number = number.multiply_by(Number(-1))

        if error:
            return res.failure(error)
        else:
            return res.success(number.set_pos(node.pos_start, node.pos_end))

# ****************************************
# RUN
# ****************************************


global_symbol_table = SymbolTable()
global_symbol_table.set("NULL", Number(0))


def run(fn, text):
    # Token generation
    lexer = Lexer(fn, text)
    tokens, error = lexer.make_token()

    if error:
        return None, error

    # Syntax Tree generation
    parser = Parser(tokens)
    ast = parser.parse()
    if ast.error:
        return None, ast.error

    # Run Program
    interpreter = Interpreter()
    context = Context('<program>')
    context.symbol_table = global_symbol_table
    result = interpreter.visit(ast.node, context)

    return result.value, result.error
