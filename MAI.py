#****************************************
# IMPORTS
#****************************************

from strings_with_arrows import *

#****************************************
# CONSTANTS
#****************************************

DIGITS = '0123456789'

#****************************************
# ERROR
#****************************************

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
        result += f'\n\n' + string_with_arrows(self.pos_start.ftxt, self.pos_start, self.pos_end)
        return result
    
class IllegalCharError(Error):
    def __init__(self, pos_start, pos_end, details):
        super().__init__(pos_start, pos_end, 'uWu...Illegal character \n Note: ', details)

class InvalidSyntaxError(Error):
    def __init__(self, pos_start, pos_end, details = ''):
        super().__init__(pos_start, pos_end, 'uWu...Invalid Syntax \nNote ', details) 


#****************************************
# POSITION
#****************************************

class Position:
    def __init__(self, idx, ln, col, fn, ftxt):
        self.idx = idx
        self.ln = ln
        self.col = col
        self.fn = fn
        self.ftxt = ftxt

    def advance(self, curr_char = None):
        self.idx += 1
        self.col += 1

        if curr_char == '\n':
            self.ln += 1
            self.col += 1
        return self
    
    def copy(self):
        return Position(self.idx, self.ln, self.col, self.fn, self.ftxt)

#****************************************
# TOKENS
#****************************************

MAI_INT = 'INT'
MAI_FLOAT = 'FLOAT'
MAI_ADD = 'ADD'
MAI_SUB = 'SUB'
MAI_MULT = 'MULT'
MAI_DIV = 'DIV'
MAI_LAPREN = 'LPAREN'
MAI_RAPREN = 'RPAREN'
MAI_EOF = 'EOF'

class Token:
    def __init__(self, type_, value = None, pos_start = None, pos_end = None):
        self.type = type_
        self.value = value
        if pos_start:
            self.pos_start = pos_start.copy()
            self.pos_end = pos_start.copy()
            self.pos_end.advance()
        if pos_end:
            self.pos_end = pos_end.copy()

    
    def __repr__(self):
        if self.value: return f"{self.type}:{self.value}"
        return f"{self.type}"

#****************************************
# LEXER
#****************************************

class Lexer:
    def __init__(self, fn, text):
        self.fn = fn
        self.text = text
        self.pos = Position(-1, 0, -1, fn, text)
        self.curr_char = None
        self.advance()

    def advance(self):
        self.pos.advance(self.curr_char)
        self.curr_char = self.text[self.pos.idx] if self.pos.idx < len(self.text) else None

    def make_token(self):
        token = []
        while self.curr_char != None:
            if self.curr_char in ' \t':
                self.advance()
            elif self.curr_char in DIGITS:
                token.append(self.make_number())
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
        
#****************************************
# NODES
#****************************************
    
class NumberNode:
	def __init__(self, tok):
		self.tok = tok

	def __repr__(self):
		return f'{self.tok}'

class BinOpNode:
    def __init__(self, left_node, op_tok, right_node):
        self.left_node = left_node
        self.op_tok = op_tok
        self.right_node = right_node
                
    def __repr__(self):
        return f'({self.left_node}, {self.op_tok}, {self.right_node})'

class UnaryOpNode:
	def __init__(self, op_tok, node):   
		self.op_tok = op_tok
		self.node = node

	def __repr__(self):
		return f'({self.op_tok}, {self.node})'

#****************************************
# PARSE RESULT
#****************************************

class ParseResult:
    def __init__(self):
        self.error = None
        self.node = None
    
    def register(self, res):
        if isinstance(res, ParseResult):
            if res.error:
                self.error = res.error
            return res.node
        return res
        
    def success(self, node):
        self.node = node
        return self
    
    def failure(self, error): 
        self.error = error
        return self

#****************************************
# PARSER
#****************************************

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
    
    def bin_op(self, func, ops):
        res = ParseResult()
        left = res.register(func())
        
        if res.error:
            return res
       
        while self.curr_tok.type in ops:
            op_tok = self.curr_tok
            res.register(self.advance())
            right= res.register(func())

            if res.error:
                return res
            left = BinOpNode(left, op_tok, right)
        return res.success(left)
    
    def parse(self):
        res = self.expr()
        if not res.error and self.curr_tok.type != MAI_EOF:
            return res.failure(InvalidSyntaxError(self.curr_tok.pos_start, self.curr_tok.pos_end, "Expected '+' '-' '*' '/'"))
        return res

    def factor(self):
        res = ParseResult()
        tok = self.curr_tok

        if tok.type in (MAI_ADD, MAI_SUB):
            res.register(self.advance())
            factor = res.register(self.factor())
            if res.error:
                return res
            return res.success(UnaryOpNode(tok, factor))

        elif tok.type in (MAI_INT, MAI_FLOAT):
            res.register(self.advance())
            return res.success(NumberNode(tok))
            
        elif tok.type == MAI_LAPREN:
            res.register(self.advance())
            expr = res.register(self.expr())
            if res.error:
                return res
            
            if self.curr_tok.type == MAI_RAPREN:
                res.register(self.advance())
                return res.success(expr)
            else:
                return res.failure(InvalidSyntaxError(self.curr_tok.pos_start, self.curr_tok.pos_end, "Expected ')'"))
            

        return res.failure(InvalidSyntaxError(tok.pos_start, tok.pos_end, "Expected INT or FLOAT"))    

    def term(self):
        return self.bin_op(self.factor, (MAI_MULT, MAI_DIV))

    def expr(self):
        return self.bin_op(self.term, (MAI_ADD, MAI_SUB))

#****************************************
# RUN
#****************************************

def run(fn, text):
    # Token generation
    lexer = Lexer(fn, text)
    tokens, error = lexer.make_token()
    
    if error:
        return None, error

    # Syntax Tree generation
    parser = Parser(tokens)
    ast = parser.parse()

    return ast.node, ast.error        