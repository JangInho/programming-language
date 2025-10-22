#include<stdio.h>
#include <stdlib.h>
#include<ctype.h>
#include <string.h>

#define MAX_TOK     256 /* 한 줄의 최대 토큰 수 */
#define MAX_VAR     256 /* 변수 종류 수 */
#define LEN_ID      10  /* <var>, <number> 최대 길이 */
#define OUTBUF_SIZE 4096 /* 출력 버퍼 사이즈 */

/* 출력 버퍼 */
static char outbuf[OUTBUF_SIZE];
static int  outpos = 0; // 버퍼에 쌓인 글자 수

/* --- 토큰 정의 --- */
typedef enum {
    TK_ID, TK_NUM,
    TK_ASSIGN, TK_PLUS, TK_MINUS, TK_MUL,
    TK_LP, TK_RP, TK_SEMI,
    TK_PRINT,
    TK_END, TK_ERR
} TokenType;

typedef struct {
    TokenType type;
    char lex[LEN_ID + 1]; // '/0'까지 고려하여 +1
} Token;

/* --- 전역 토큰 리스트 & 인덱스 --- */
static Token tok[MAX_TOK];
static int pos, toklen;

/* 심볼 테이블 */
typedef struct { 
    char name[LEN_ID + 1]; long long val; 
} Var;
static Var sym[MAX_VAR];
static int  symcnt = 0;

/* 
* 심볼테이블에서 변수(이름)에 대응하는 값을 찾거나,
* 없다면 새로 항목을 만들어 0으로 초기화한 뒤
* 그 값이 저장된 메모리 주소를 돌려준다.
*/
static long long* lookup(const char *name)
{
    for (int i = 0; i < symcnt; ++i)
        if (strcmp(sym[i].name, name) == 0)
            return &sym[i].val;
    if (symcnt == MAX_VAR) return NULL; // 변수 갯수 제한 
    strncpy(sym[symcnt].name, name, LEN_ID);
    sym[symcnt].val = 0; // 기본값 0
    return &sym[symcnt++].val;
}


/* 토큰 헬퍼 함수 */
static Token* ctok(void) { 
    return (pos < toklen) ? &tok[pos] : &tok[toklen - 1]; 
}
static int accept(TokenType t) {
    if (ctok()->type == t) { 
        ++pos; return 1; 
    } 
    return 0; 
}
static int expect(TokenType t) { 
    if (accept(t)) 
        return 1; 
    return 0; 
}

/* ---------- 파서 선언 ---------- */
static int parse_program(void);
static int parse_statement(int *print_pending);
static int parse_expr(long long *v);
static int parse_term(long long *v);
static int parse_factor(long long *v);

/* <program> → {<statement>} */
static int parse_program(void)
{

    int ok = 1, print_pending = 0;
    outpos = 0;                         /* 이번 줄용 버퍼 리셋 */

    while (pos < toklen - 1 && ok)  // 마지막 토큰은 TK_END
        ok = parse_statement(&print_pending);

    if (ok && print_pending) {          /* 정상 종료 → 버퍼 flush */
        outbuf[outpos++] = '\n';
        outbuf[outpos]   = '\0';
        fputs(outbuf, stdout);
    }

    return ok;
}

/* <statement> → <var> = <expr> ; | print <var> ; */
static int parse_statement(int *pp)
{
    Token *t = ctok();
    if (t->type == TK_ID) {
        
        char id[LEN_ID + 1]; 
        strncpy(id, t->lex, LEN_ID); 
        accept(TK_ID);

        if (!expect(TK_ASSIGN)) return 0;

        long long val;
        if (!parse_expr(&val)) return 0;
        if (!expect(TK_SEMI)) return 0;
        long long *p = lookup(id);
        if (!p) return 0;
        *p = val;
        return 1;
    }
    if (t->type == TK_PRINT) {
        accept(TK_PRINT);
        if (ctok()->type != TK_ID) return 0;
        long long *p = lookup(ctok()->lex);
        if (!p) return 0;
        accept(TK_ID);
        if (!expect(TK_SEMI)) return 0;

        if (!*pp) {  // 첫 print 앞에는 '>> ' 
            outpos += snprintf(outbuf + outpos,
                                OUTBUF_SIZE - outpos,
                                ">> %lld", *p);
        } else { // 두 번째부터는 공백으로 구분 
            outpos += snprintf(outbuf + outpos,
                               OUTBUF_SIZE - outpos,
                               " %lld", *p);
        }

        *pp = 1;
        return 1;
    }
    return 0;   /* 둘 다 아님 → 오류 */
}

/* <expr> → <term> {+ <term> | * <term>} */
static int parse_expr(long long *v)
{
    if (!parse_term(v)) return 0;
    while (ctok()->type == TK_PLUS || ctok()->type == TK_MUL) {
        TokenType op = ctok()->type; accept(op);
        long long rhs;
        if (!parse_term(&rhs)) return 0;
        if (op == TK_PLUS) 
            *v = *v + rhs;
        else 
            *v = *v * rhs;
    }
    return 1;
}

/* <term> → <factor> {- <factor>} */
static int parse_term(long long *v)
{
    if (!parse_factor(v)) return 0;
    while (ctok()->type == TK_MINUS) {
        accept(TK_MINUS);
        long long rhs;
        if (!parse_factor(&rhs)) return 0;
        *v = *v - rhs;
    }
    return 1;
}

 /* <factor> → [ - ] ( <number> | <var> | ‘(’<expr>‘)’ ) */
static int parse_factor(long long *v)
{
    int neg = accept(TK_MINUS);
    Token *t = ctok();

    if (t->type == TK_NUM) {
        *v = atoll(t->lex);
        accept(TK_NUM);
    } else if (t->type == TK_ID) {
        long long *p = lookup(t->lex);
        if (!p) return 0;
        *v = *p ? *p : 0;
        accept(TK_ID);
    } else if (accept(TK_LP)) {
        if (!parse_expr(v)) return 0;
        if (!expect(TK_RP)) return 0;
    } else {
        return 0;
    }

    if (neg) *v = -(*v);
    return 1;
}



/* ---- lexical analysis --- */
static TokenType classify(const char *s)
{
    if (strcmp(s, "=")  == 0) return TK_ASSIGN;
    if (strcmp(s, "+")  == 0) return TK_PLUS;
    if (strcmp(s, "-")  == 0) return TK_MINUS;
    if (strcmp(s, "*")  == 0) return TK_MUL;
    if (strcmp(s, "(")  == 0) return TK_LP;
    if (strcmp(s, ")")  == 0) return TK_RP;
    if (strcmp(s, ";")  == 0) return TK_SEMI;
    if (strcmp(s, "print") == 0) return TK_PRINT;

    if (isdigit(*s)) {
        for (int i = 0; s[i]; ++i) 
            if (!isdigit(s[i]) || i >= LEN_ID) 
                return TK_ERR;
        return TK_NUM;
    }
    if (islower(*s)) {
        for (int i = 0; s[i]; ++i) 
            if (!islower(s[i]) || i >= LEN_ID) 
                return TK_ERR;
        return TK_ID;
    }
    return TK_ERR;
}

static int tokenize(char *line)
{
    toklen = 0; pos = 0;
    char *p = strtok(line, " \t\r\n");
    while (p && toklen < MAX_TOK - 1) {
        tok[toklen].type = classify(p);
        strncpy(tok[toklen].lex, p, LEN_ID);
        tok[toklen++].lex[LEN_ID] = '\0';
        p = strtok(NULL, " \t\r\n");
    }
    tok[toklen].type = TK_END; // 방어 처리
    return (toklen > 1); // 최소 1개 이상 토큰
}

int main(void) {

    char line[1024]; // 최대 1023문자 + 널 까지 읽도록
    while (1) {
        printf(">> ");
        fflush(stdout);
        if (!fgets(line, sizeof line, stdin)) break;          /* EOF */
        if (line[0] == '\n' || line[0] == '\0') break;        /* 빈 줄 → 종료 */

        if (!tokenize(line)) { puts(">> Syntax Error!"); continue; }

        if (!parse_program()) puts(">> Syntax Error!");

        symcnt = 0; // 한 줄 당 기억해뒀던 변수 날리기
    }
    return 0;
}
