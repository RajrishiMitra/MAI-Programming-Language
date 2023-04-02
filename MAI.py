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
        return result
    
class IllegalCharError(Error):
    def __init__(self, pos_start, pos_end, details):
        super().__init__(pos_start, pos_end, 'Illegal character', details)

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

    def advance(self, curr_char):
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

MAI_INT = 'MAI_INT'
MAI_FLOAT = 'MAI_FLOAT'
MAI_ADD = 'MAI_ADD'
MAI_SUB = 'MAI_SUB'
MAI_MULT = 'MAI_MULT'
MAI_DIV = 'MAI_DIV'
MAI_LAPREN = 'MAI_LPAREN'
MAI_RAPREN = 'MAI_RPAREN'

class Token:
    def __init__(self, type_, value = None):
        self.type = type_
        self.value = value
    
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
                token.append(Token(MAI_ADD))
                self.advance()
            elif self.curr_char == '-':
                token.append(Token(MAI_SUB))
                self.advance()
            elif self.curr_char == '*':
                token.append(Token(MAI_MULT))
                self.advance()
            elif self.curr_char == '/':
                token.append(Token(MAI_DIV))
                self.advance()
            elif self.curr_char == '(':
                token.append(Token(MAI_LAPREN))
                self.advance()
            elif self.curr_char == ')':
                token.append(Token(MAI_RAPREN))
                self.advance()
            else:
                pos_start = self.pos.copy()
                char = self.curr_char
                self.advance()
                return [], IllegalCharError(pos_start, self.pos, "'" + char + "'")
        return token, None

    def make_number(self):
        num_str = ''
        dot_count = 0
        #pos_start = self.pos.copy()

        while self.curr_char != None and self.curr_char in DIGITS + '.':
            if self.curr_char == '.':
                if dot_count == 1: 
                    break
                dot_count += 1
            num_str += self.curr_char
            self.advance()

        if dot_count == 0:
            return Token(MAI_INT, int(num_str))
        else:
            return Token(MAI_FLOAT, float(num_str))
            
#****************************************
# RUN
#****************************************

def run(fn, text):
    lexer = Lexer(fn, text)
    tokens, error = lexer.make_token()
    return tokens, error        