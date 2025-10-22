#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_TOK  2048
#define MAX_VAR  256
#define LEN_ID   10
#define OUTBUF_SIZE 4096

// 토큰 종류
typedef enum {
    TK_ID, TK_NUM,
    TK_INTEGER,
    TK_ASSIGN, TK_PLUS, TK_MINUS, TK_MUL,
    TK_LP, TK_RP, TK_LB, TK_RB, TK_SEMI,
    TK_PRINT, TK_WHILE, TK_DO, TK_IF, TK_ELSE,
    TK_EQ, TK_NEQ, TK_LT, TK_GT,
    TK_END, TK_ERR
} TokenType;

typedef struct { 
    TokenType type; 
    char lex[LEN_ID + 1]; 
} Token;
static Token tok[MAX_TOK];  
static int pos, toklen;

// symbol table
typedef struct { 
    char name[LEN_ID + 1]; 
    long long val; 
} Var;
static Var sym[MAX_VAR];    
static int symcnt;

// print buffer
static char outbuf[OUTBUF_SIZE]; 
static int outpos;

// helper 함수
static Token *ctok(void){ 
    return (pos < toklen) ? &tok[pos] : &tok[toklen - 1]; 
}
static int accept(TokenType t){ 
    if (ctok()->type == t){ ++pos; return 1; } return 0; 
}
static int expect(TokenType t){ 
    return accept(t); 
}

static Var *find_var(const char *n){ 
    for(int i=0;i<symcnt;++i) 
        if(!strcmp(sym[i].name,n)) 
            return &sym[i]; 
        return NULL;
}
static int  declare_var(const char *n){ 
    if(symcnt==MAX_VAR||find_var(n)) return 0; 
    strncpy(sym[symcnt].name,n,LEN_ID); 
    sym[symcnt].val=0; 
    ++symcnt; 
    return 1; 
}

static int parse_program(void);
static int parse_statement(int*);
static int parse_block(int*);
static void skip_block(void);
static int parse_aexpr(long long*);
static int parse_term (long long*);
static int parse_factor(long long*);
static int parse_bexpr(int*);
static int parse_bexpr_full(Var**, Var**, TokenType*);

// relop
static int rel(long long l,long long r,TokenType op){
    switch(op){ 
        case TK_EQ: 
            return l==r; 
        case TK_NEQ: 
            return l!=r;
        case TK_LT: 
            return l<r;  
        case TK_GT:  
            return l>r; 
        default: 
            return 0;
    }
    return 0;
}

const char* getTokenTypeString(TokenType type) {
    switch (type) {
        case TK_ID:       
            return "TK_ID";
        case TK_NUM:      
            return "TK_NUM";
        case TK_INTEGER:  
            return "TK_INTEGER";
        case TK_ASSIGN:   
            return "TK_ASSIGN";
        case TK_PLUS:     
            return "TK_PLUS";
        case TK_MINUS:    
            return "TK_MINUS";
        case TK_MUL:      
            return "TK_MUL";
        case TK_LP:       
            return "TK_LP";
        case TK_RP:       
            return "TK_RP";
        case TK_LB:       
            return "TK_LB";
        case TK_RB:       
            return "TK_RB";
        case TK_SEMI:     
            return "TK_SEMI";
        case TK_PRINT:    
            return "TK_PRINT";
        case TK_WHILE:    
            return "TK_WHILE";
        case TK_DO:       
            return "TK_DO";
        case TK_IF:       
            return "TK_IF";
        case TK_ELSE:     
            return "TK_ELSE";
        case TK_EQ:       
            return "TK_EQ";
        case TK_NEQ:      
            return "TK_NEQ";
        case TK_LT:       
            return "TK_LT";
        case TK_GT:       
            return "TK_GT";
        case TK_END:      
            return "TK_END";
        case TK_ERR:      
            return "TK_ERR";
        default:          
            return "UNKNOWN_TOKEN_TYPE";
    }
}


// 파싱
// program
static int parse_declaration(void){
    if(!expect(TK_INTEGER)) 
        return 0;
    if(ctok()->type!=TK_ID) 
        return 0;
    if(!declare_var(ctok()->lex)) 
        return 0;
    accept(TK_ID);
    return expect(TK_SEMI);
}

static int parse_program(void){
    int ok=1, pp=0; outpos=0;
    while(ctok()->type==TK_INTEGER && ok) 
        ok=parse_declaration();

    while(pos<toklen-1 && ok) 
        ok=parse_statement(&pp);

    if(ok && pp){ 
        outbuf[outpos++]='\n'; 
        outbuf[outpos]='\0'; 
        fputs(outbuf,stdout); 
    }
    return ok;
}

// statement
static int parse_statement(int *pp){
    // 빈 문장 
    if(accept(TK_SEMI))
        return 1;

    // 대입
    if(ctok()->type==TK_ID){
        Var *v=find_var(ctok()->lex); 
        if(!v) 
            return 0;
        
        accept(TK_ID);
        if(!expect(TK_ASSIGN)) 
            return 0;

        long long val; 
        if(!parse_aexpr(&val)) 
            return 0;
        
        if(!expect(TK_SEMI)) 
            return 0;
        
        v->val = val; 
        return 1;
    }

    // print 
    if(accept(TK_PRINT)){
        long long val; 
        if(!parse_aexpr(&val)) 
            return 0;

        if(!expect(TK_SEMI)) 
            return 0;

        if(!*pp) 
            outpos+=snprintf(outbuf+outpos,OUTBUF_SIZE-outpos,">> %lld",val);
        else     
            outpos+=snprintf(outbuf+outpos,OUTBUF_SIZE-outpos," %lld", val);
        *pp=1; 
        return 1;
    }

    // while (  ) do { ... } ; 
    if(accept(TK_WHILE)){
        if(!expect(TK_LP))
            return 0;
        Var *lhs,*rhs; 
        TokenType rop;

        if(!parse_bexpr_full(&lhs,&rhs,&rop)) 
            return 0;

        if(!expect(TK_RP)||!expect(TK_DO)||!expect(TK_LB)) 
            return 0;

        int body_start = pos;
        int depth = 1;
        int i = body_start;

        while (i < toklen && depth) {
            if (tok[i].type == TK_LB) 
                ++depth;
            else if (tok[i].type == TK_RB) 
                --depth;
            ++i;       
        }
        if (depth) 
            return 0; // 닫는 }를 못 찾음 
        int body_end = i - 1; // 바로 직전 토큰이 '}'

        while(rel(lhs->val,rhs->val,rop)){
            pos=body_start;
            while(pos<body_end) 
                if(!parse_statement(pp)) 
                    return 0;
        }
        pos=body_end; 
        if(!expect(TK_RB)) 
            return 0;

        if(!expect(TK_SEMI)) 
            return 0;  //  반드시 1개
        while(accept(TK_SEMI)){
            // 추가 세미콜론 무시
        }
        return 1;
    }

    // if (  ) { ... } else { ... } ; 
    if(accept(TK_IF)){
        if(!expect(TK_LP))
            return 0;
        int cond; 
        if(!parse_bexpr(&cond)) 
            return 0;
        if(!expect(TK_RP)||!expect(TK_LB)) 
            return 0;

        if(cond){ 
            if(!parse_block(pp)) 
                return 0; 
        } else 
            skip_block();

        if(!expect(TK_ELSE)||!expect(TK_LB)) 
            return 0;
        if(!cond){ 
            if(!parse_block(pp)) 
                return 0; 
        } else 
            skip_block();

        if(!expect(TK_SEMI)) 
            return 0;
        return 1;
    }

    return 0;  
}

// { , } 블럭
static int parse_block(int *pp){
    while(ctok()->type!=TK_RB && ctok()->type!=TK_END)
        if(!parse_statement(pp)) 
            return 0;
    return expect(TK_RB);
}
static void skip_block(void){
    int depth=1;
    while(pos<toklen && depth){
        if(tok[pos].type==TK_LB) ++depth;
        else if(tok[pos].type==TK_RB) --depth;
        
        ++pos;
    }
}

// boolean
static int parse_bexpr_full(Var **lhs,Var **rhs,TokenType *rop){
    if(ctok()->type!=TK_ID) return 0;
    *lhs=find_var(ctok()->lex); 
    if(!*lhs) 
        return 0; 
    accept(TK_ID);
    *rop=ctok()->type;
    if(!(*rop==TK_EQ||*rop==TK_NEQ||*rop==TK_LT||*rop==TK_GT)) 
        return 0;
    accept(*rop);
    if(ctok()->type!=TK_ID) 
        return 0;
    *rhs=find_var(ctok()->lex); 
    if(!*rhs) 
        return 0; 
    accept(TK_ID);
    return 1;
}
static int parse_bexpr(int *bv){
    Var *l,*r; 
    TokenType op;
    if(!parse_bexpr_full(&l,&r,&op)) 
        return 0;
    *bv=rel(l->val,r->val,op); 
    return 1;
}

// 산술 연산
static int parse_aexpr(long long *v){
    if(!parse_term(v)) return 0;
    while(ctok()->type==TK_PLUS||ctok()->type==TK_MINUS){
        TokenType op=ctok()->type; 
        accept(op);
        long long rhs; 
        if(!parse_term(&rhs)) 
            return 0;
        if(op==TK_PLUS)
            *v+=rhs; 
        else 
            *v-=rhs;
    }
    return 1;
}

static int parse_term(long long *v){
    if(!parse_factor(v)) 
        return 0;
    while(ctok()->type==TK_MUL){
        accept(TK_MUL);
        long long rhs; 
        if(!parse_factor(&rhs)) 
            return 0;
        *v*=rhs;
    }
    return 1;
}

static int parse_factor(long long *v){
    int neg=accept(TK_MINUS);
    if(ctok()->type==TK_NUM){ 
        *v=atoll(ctok()->lex); accept(TK_NUM); 
    } else if(ctok()->type==TK_ID){
        Var *p=find_var(ctok()->lex); 

        if(!p) return 0;

        *v=p->val; 
        accept(TK_ID);
    }
    else if(accept(TK_LP)){
         if(!parse_aexpr(v)||!expect(TK_RP)) 
            return 0; 
    } else return 0;

    if(neg) *v=-*v; 
        return 1;
}

// - 가 종류가 여러개여서 생기는 문제가 있어서 추가함
static int is_unicode_dash(const char *s){
    return !strcmp(s, "\xE2\x80\x93") ||   /* – */
           !strcmp(s, "\xE2\x80\x94") ||   /* — */
           !strcmp(s, "\xE2\x88\x92");     /* − */
}


static TokenType classify(const char *s){
    if(!strcmp(s,"=")) return TK_ASSIGN;
    if(!strcmp(s,"+")) return TK_PLUS;
    if(!strcmp(s,"-") || is_unicode_dash(s) ) return TK_MINUS;
    if(!strcmp(s,"*")) return TK_MUL;
    if(!strcmp(s,"(")) return TK_LP;
    if(!strcmp(s,")")) return TK_RP;
    if(!strcmp(s,"{")) return TK_LB;
    if(!strcmp(s,"}")) return TK_RB;
    if(!strcmp(s,";")) return TK_SEMI;
    if(!strcmp(s,"print")) return TK_PRINT;
    if(!strcmp(s,"while")) return TK_WHILE;
    if(!strcmp(s,"do")) return TK_DO;
    if(!strcmp(s,"if")) return TK_IF;
    if(!strcmp(s,"else")) return TK_ELSE;
    if(!strcmp(s,"integer")) return TK_INTEGER;
    if(!strcmp(s,"==")) return TK_EQ;
    if(!strcmp(s,"!=")) return TK_NEQ;
    if(!strcmp(s,"<")) return TK_LT;
    if(!strcmp(s,">")) return TK_GT;

    if(isdigit((unsigned char)s[0])){
        for(int i=0;s[i];++i) 
            if(!isdigit((unsigned char)s[i])||i>=LEN_ID) 
                return TK_ERR;
        return TK_NUM;
    }
    if(islower((unsigned char)s[0])){
        for(int i=0;s[i];++i) 
            if(!islower((unsigned char)s[i])||i>=LEN_ID) 
                return TK_ERR;
        return TK_ID;
    }
    return TK_ERR;
}

static int tokenize(char *line){
    toklen=pos=0;

    for(char *p=strtok(line," \t\r\n"); p && toklen<MAX_TOK-1;
        p=strtok(NULL," \t\r\n")){
        tok[toklen].type=classify(p);
        
        strncpy(tok[toklen].lex,p,LEN_ID);
        tok[toklen].lex[LEN_ID]='\0';
        ++toklen;
    }
    tok[toklen].type=TK_END;
    
    return toklen>1;
}

int main(void){
    char line[8192];
    while(1){
        printf(">> "); 
        fflush(stdout);
        if(!fgets(line,sizeof line,stdin)) break;
        if(line[0]=='\n'||line[0]=='\0') break;

        symcnt=0; // refresh
                        
        if(!tokenize(line)||!parse_program())
            puts(">> Syntax Error!");
    }
    return 0;
}

// integer k ; integer j ; k = 30 ; j = 25 ; while ( k > j ) do { print ( k - j ) * 10 ; k = k - 1 ; } ; print k ; 
// integer k ; integer j ; k = 30 ; j = 25 ; while ( k > j ) do { print ( k - j ) * 10 ; k = k - 1 ; } ; print k ;
// integer x ; integer y ; x = 10 + 5 * 2 ; y = 20 - 5 ; if ( x < y ) { print x - 5 ; } else { x = 7 ; print x + 5 ; } ;
// integer x ; integer y ; x = 10 + 5 * 2 ; y = 20 - 5 ; if ( x < y ) { print x - 5 ; } else { x = 7 ; print x + 5 : } ;

// integer i ; integer j ; integer k ; i = 0 ; j = 5 ; k = 3 ; while ( i < j ) do { if ( i < k ) { i = i + 1 ; print i ; } else { i = i + 1 ; } ; } ; print i ; print j * k ;
