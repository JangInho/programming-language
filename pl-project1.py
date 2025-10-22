from sys import stdout

MAX_ID_LEN = 10
TOK_END = ("END", "")

# ────────────────── lexical anlaysis ──────────────────
def classify(tok: str):
    """토큰 문자열을 (타입, 렉심) 튜플로 변환. 오류 시 None."""
    if tok in {"=", "+", "-", "*", "(", ")", ";"}:
        return (tok, tok)
    if tok == "print":
        return ("PRINT", tok)
    if tok.isdigit() and len(tok) <= MAX_ID_LEN:
        return ("NUM", tok)
    if tok.islower() and tok.isalpha() and len(tok) <= MAX_ID_LEN:
        return ("ID", tok)
    return None


def tokenize(line: str):
    parts = line.strip().split()
    if not parts:
        return [] # 빈 줄
    toks = [classify(p) for p in parts]
    if None in toks:
        return None # 토큰화 실패
    toks.append(TOK_END) # 방어 토큰
    return toks


# ────────────────── 파서 (RD) ──────────────────
class Parser:
    def __init__(self, tokens):
        self.toks = tokens
        self.pos = 0
        self.sym = {} # 줄마다 새로 생성
        self.out_parts = []  # print 결과를 임시 저장

    # 헬퍼
    def peek(self):
        return self.toks[self.pos]

    def accept(self, kind):
        if self.peek()[0] == kind:
            self.pos += 1
            return True
        return False

    def expect(self, kind):
        if self.accept(kind):
            return True
        raise SyntaxError

    # <program> → {<statement>}
    def program(self):
        # self.print_pending = False  # 줄 시작마다 리셋
        while self.peek()[0] != "END":
            # self.statement()  # 플래그는 인자로 넘길 필요 없음
        # if self.print_pending:  # print가 있었다면 개행
        #     print()
            self.statement()
        if self.out_parts:  # 구문오류 없이 끝났다면
            print("".join(self.out_parts))

        return True

    # <statement> → <var> = <expr> ; | print <var> ;
    def statement(self):
        tokkind, toklex = self.peek()
        if tokkind == "ID": # assignment
            name = toklex
            self.accept("ID")
            self.expect("=")
            val = self.expr()
            self.expect(";")
            self.sym[name] = val
            return True
        elif tokkind == "PRINT": # print-statement
            self.accept("PRINT")
            if self.peek()[0] != "ID":
                raise SyntaxError
            name = self.peek()[1]
            self.accept("ID")
            self.expect(";")
            val = self.sym.get(name, 0)  # 기본 0

            if not self.out_parts:  # 첫 print
                self.out_parts.append(">> " + str(val))
            else:  # 두 번째부터
                self.out_parts.append(" " + str(val))

            return True
        raise SyntaxError

    # <expr> → <term> {+ <term> | * <term>}
    def expr(self):
        v = self.term()
        while self.peek()[0] in {"+", "*"}:
            op = self.peek()[0]
            self.accept(op)
            rhs = self.term()
            v = v + rhs if op == "+" else v * rhs
        return v

    # <term> → <factor> {- <factor>}
    def term(self):
        v = self.factor()
        while self.peek()[0] == "-":
            self.accept("-")
            rhs = self.factor()
            v = v - rhs
        return v

    # <factor> → [ - ] ( <number> | <var> | ‘(’<expr>‘)’ )
    def factor(self):
        neg = self.accept("-")
        kind, lex = self.peek()
        if kind == "NUM":
            self.accept("NUM")
            val = int(lex)
        elif kind == "ID":
            self.accept("ID")
            val = self.sym.get(lex, 0)
        elif self.accept("("):
            val = self.expr()
            self.expect(")")
        else:
            raise SyntaxError
        return -val if neg else val


# ────────────────── 메인 ──────────────────
def repl():
    while True:
        try:
            line = input(">> ")
        except EOFError:
            break
        if not line.strip(): # 빈 줄 → 종료
            break
        tokens = tokenize(line)
        if tokens is None:
            print(">> Syntax Error!")
            continue
        try:
            Parser(tokens).program()
        except SyntaxError:
            print(">> Syntax Error!")


if __name__ == "__main__":
    repl()