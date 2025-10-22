
MAX_ID_LEN = 10
TOK_END = ("END", "")

# 전역
sym: dict[str, int]   = {}
declared: set[str]    = set()

#  토큰화
def classify(tok: str):
    # 토큰을 튜플로 처리
    # 한글자 기호
    if tok in {"=", "+", "-", "–", "−", "*", "(", ")", ";", "{", "}"}:
        return ("-" if tok in {"–", "−"} else tok, tok)
    # 2글자 비교
    if tok in {"==", "!=", "<", ">"}:
        return (tok, tok)
    # 키워드
    if tok in {"print", "integer", "while", "do", "if", "else"}:
        return (tok.upper(), tok)
    # 숫자 변수
    if tok.isdigit() and len(tok) <= MAX_ID_LEN:
        return ("NUM", tok)
    if tok.isalpha() and tok.islower() and len(tok) <= MAX_ID_LEN:
        return ("ID", tok)
    return None


def tokenize(line: str):
    parts = line.strip().split()
    if not parts:
        return []
    toks = [classify(p) for p in parts]
    if None in toks:
        return None
    toks.append(TOK_END)
    return toks

# 파서
class Parser:
    def __init__(self, tokens, out_parts, execute=True):
        self.toks, self.pos = tokens, 0
        self.out_parts, self.execute = out_parts, execute

    # 토큰 헬퍼
    def peek(self): return self.toks[self.pos]
    def accept(self, k):
        if self.peek()[0] == k:
            self.pos += 1
            return True
        return False
    def expect(self, k):
        if not self.accept(k):
            raise SyntaxError

    # <program>
    def program(self):
        while self.peek()[0] == "INTEGER":
            self.declaration()
        while self.peek()[0] != "END":
            self.statement()

    # <declaration> → integer <var> ;
    def declaration(self):
        self.expect("INTEGER")
        if self.peek()[0] != "ID": raise SyntaxError
        name = self.peek()[1]
        self.accept("ID")
        self.expect(";")
        if self.execute:
            declared.add(name)
            sym[name] = 0 # 항상 재초기화

    # <statement>
    def statement(self):
        k = self.peek()[0]
        if k == "ID":
            self.assign()
        elif k == "PRINT":
            self.do_print()
        elif k == "WHILE":
            self.do_while()
        elif k == "IF":
            self.do_if()
        else:
            raise SyntaxError

    def assign(self):
        name = self.peek()[1]
        if name not in declared: raise SyntaxError
        self.accept("ID")
        self.expect("=")
        val = self.aexpr()
        self.expect(";")
        if self.execute: sym[name] = val

    # 2) print <aexpr> ;
    def do_print(self):
        self.expect("PRINT")
        val = self.aexpr()
        self.expect(";")
        if self.execute:
            self.out_parts.append((" " if self.out_parts else "") + str(val))

    # 3) while ( <bexpr> ) do { … } ;
    def do_while(self):
        self.expect("WHILE"); self.expect("(")
        cond = self.collect_until(")"); self.expect(")")
        self.expect("DO"); block = self.collect_block(); self.expect(";")

        # 문법검사
        Parser(cond+[TOK_END], [], False).bexpr()
        Parser(block+[TOK_END], [], False).program()

        while self.execute and self.eval_bexpr(cond):
            Parser(block+[TOK_END], self.out_parts).program()

    # 4) if ( <bexpr> ) { … } else { … } ;
    def do_if(self):
        self.expect("IF"); self.expect("(")
        cond = self.collect_until(")"); self.expect(")")
        then_blk = self.collect_block(); self.expect("ELSE")
        else_blk = self.collect_block(); self.expect(";")

        Parser(cond+[TOK_END], [], False).bexpr()
        Parser(then_blk+[TOK_END], [], False).program()
        Parser(else_blk+[TOK_END], [], False).program()

        tgt = then_blk if self.execute and self.eval_bexpr(cond) else else_blk
        if self.execute:
            Parser(tgt+[TOK_END], self.out_parts).program()

    def bexpr(self):
        l_name = self.require_id()
        op = self.peek()[0]
        if op not in {"==", "!=", "<", ">"}:
            raise SyntaxError
        self.accept(op)
        r_name = self.require_id()
        if not self.execute: return False
        return {"==": sym[l_name]==sym[r_name],
                "!=": sym[l_name]!=sym[r_name],
                "<":  sym[l_name]< sym[r_name],
                ">":  sym[l_name]> sym[r_name]}[op]

    def eval_bexpr(self, tks): return Parser(tks+[TOK_END], [], True).bexpr()

    def aexpr(self):
        v = self.term()
        while self.peek()[0] in {"+", "-"}:
            op = self.peek()[0]; self.accept(op)
            rhs = self.term(); v = v+rhs if op=="+" else v-rhs
        return v

    def term(self):
        v = self.factor()
        while self.peek()[0] == "*":
            self.accept("*"); v *= self.factor()
        return v

    def factor(self):
        neg = self.accept("-")
        if self.peek()[0] == "NUM":
            val = int(self.peek()[1]); self.accept("NUM")
        elif self.peek()[0] == "ID":
            name = self.require_id(); val = sym[name]
        elif self.accept("("):
            val = self.aexpr(); self.expect(")")
        else: raise SyntaxError
        return -val if neg else val

    def require_id(self):
        if self.peek()[0] != "ID" or self.peek()[1] not in declared:
            raise SyntaxError
        name = self.peek()[1];
        self.accept("ID");
        return name

    def collect_until(self, stop):
        saved = self.pos; buf=[]
        while self.toks[self.pos][0]!=stop:
            if self.peek()[0]=="END": raise SyntaxError
            buf.append(self.peek()); self.pos+=1
        self.pos = saved
        while self.toks[self.pos][0]!=stop:
            buf.append(self.peek()); self.pos+=1
        return buf

    def collect_block(self):
        self.expect("{"); lvl, start = 1, self.pos
        while lvl:
            tk = self.peek()
            if tk[0]=="{": lvl+=1
            elif tk[0]=="}": lvl-=1
            if lvl==0: break
            if tk[0]=="END": raise SyntaxError
            self.pos+=1
        end = self.pos; self.expect("}")
        return self.toks[start:end]

def repl():
    while True:
        try:
            line = input(">> ")
        except EOFError:
            break
        if not line.strip():
            break
        toks = tokenize(line)
        if toks is None:
            print(">> Syntax Error!");
            continue
        out_parts=[]
        try:
            Parser(toks, out_parts).program()
            if out_parts: print(">> " + "".join(out_parts))
        except SyntaxError:
            print(">> Syntax Error!")

if __name__ == "__main__":
    repl()
