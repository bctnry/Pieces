#include<ctype.h>
#include<string.h>
#include<stdio.h>
#include<stdlib.h>
#include<math.h>

/* gcc --std=c89 [filename] -lm */
/* if there's warning about using gets don't worry you just don't
 * get gets in c89. it's for the driver only; the parser itself does not rely
 * on that. */

/* ------------------------------------------------------------- */
/* save the contents below as archlisp_parser.h & replace this
 * block with #include"archlisp_parser.h" for good measure.      */

/* respectively space, \n, \r, and \t.
 * i'm afriad that writing \n& stuff as-is might make some compilers
 * unhappy...
 */
#define iswhitespace(T) (T==' '||T==10||T==13||T==9)
#define setErrorneousResultAt(T) T.type=ERROR,T.data=NULL,T.rest=NULL
#define hexd2int(T) (T>='a'&&T<='f'?T-'a'+10:(T>='A'&&T<='F'?T-'A'+10:T-'0'));
#define newNode malloc(sizeof(Node))
#define newNodeListNode malloc(sizeof(NodeListNode))
#define reportErrorWith(T) putchar((char)10);\
    printf("Unexpected symbol at row %d, col %d.",row,col);\
    putchar((char)10);\
    {\
        T\
        ;\
    }

/* int is by default long long (i.e. 64bit)
 * float is by default double (i.e. also 64bit)
 * string is bytestring. the following escape sequences are supported:
 *     \" \\ \b \f \n \r \t \xAA where AA is hexadecimal digits.
 * identifier? identifier ::= [a-zA-Z_.=+/*-+~`$%^&:?][a-zA-Z0-9_.=+/*-+~`$%^&:?]*
 * bang-identifier means identifiers that begin with an !, which might
 * be reserved for strict functions if archlisp becomes lazy by default
 * in the future; at-identifier means the same thing except that they
 * begin with an @; those identifiers are reserved for macros. probably
 * unhygienic macros.
 */
/* data Node = LIST NodeList
//           | STRING_LITERAL String
//           | INT_LITERAL Int
//           | FLOAT_LITERAL Float
//           | IDENTIFIER String
//           | BANG_IDENTIFIER String
//           | AT_IDENTIFIER String
//           | SYMBOL String
//           | !@#
*/
typedef enum {
    LIST,
    STRING_LITERAL, INT_LITERAL, FLOAT_LITERAL,
    IDENTIFIER,
    BANG_IDENTIFIER, AT_IDENTIFIER, SYMBOL
} NodeType;
typedef struct {
    NodeType type;
    void* data;
} Node;
/*  type NodeList = [Node]  */
typedef struct {
    void* data;
    void* next;
} NodeListNode;

/* data TokenizeResult = ERROR () | VALID_TOKEN Node String */
typedef enum {
    ERROR, VALID_TOKEN
} TokenizeResultTag;
typedef struct {
    TokenizeResultTag type;
    Node* data;
    char* rest;
} TokenizeResult;
/* ------------------------------------------------------------- */

int row = 1, col = 1;

void consumeWhitespace(char** str){
    while(iswhitespace(str[0][0])){
        if(str[0][0]=='\n'){col=1,row++;}else{col++;}
        (*str)++;
    }
}
TokenizeResult isString(char* str){
    TokenizeResult res;
    Node* resn;
    char* stringres;
    int i=0; int length=0; int j=0; int k=0; int charbuf1,charbuf2;
    if(str[0]!='\"'){
        setErrorneousResultAt(res); return res;
    }
    i++; col++;
    while(str[i]){
        if(str[i]=='\\'){
            if(!str[i+1]){setErrorneousResultAt(res); return res;}
            else{
                switch(str[i+1]){
                    case '\"': case '\\': case 'b':
                    case 'f': case 'n': case 'r': case 't':
                    { length++; i+=2; col+=2; break; }
                    case 'x':{
                        if(str[i+2]&&isxdigit(str[i+2])
                            && str[i+3]&&isxdigit(str[i+3])
                        ){ length++; i+=4; col+=4; break; }
                        else {
                            setErrorneousResultAt(res); return res;
                        }
                    }
                    default:{
                        setErrorneousResultAt(res); return res;
                    }
                }
            }
        }
        else if(str[i]!='\"'){
            length++; i++; col++;
        }
        else if(str[i]=='\"'){
            stringres = malloc(length);
            j=1; while(str[j]&&str[j]!='\"'){
                if(str[j]=='\\'){
                    switch(str[j+1]){
                        case '\"':{ stringres[k++] = '\"'; j+=2; break; }
                        case '\\':{ stringres[k++] = '\\'; j+=2; break; }
                        case 'b':{ stringres[k++] = '\b'; j+=2; break; }
                        case 'f':{ stringres[k++] = '\f'; j+=2; break; }
                        case 'n':{ stringres[k++] = '\n'; j+=2; break; }
                        case 'r':{ stringres[k++] = '\r'; j+=2; break; }
                        case 't':{ stringres[k++] = '\t'; j+=2; break; }
                        case 'x':{
                            charbuf1 = hexd2int(str[j+2]);
                            charbuf2 = hexd2int(str[j+3]);
                            stringres[k++] = charbuf1*16+charbuf2;
                            j+=4;
                            break;
                        }
                    }
                }else{
                    stringres[k++] = str[j++];
                }
            }
            /* TODO: check if this actually works
             * i'm not sure about this one i'm bad at identifying postconds
             * of loops
             */
            col++;
            res.type = VALID_TOKEN;
            resn = newNode;
            resn->type = STRING_LITERAL;
            resn->data = (void*)stringres;
            res.rest = str+i+1;
            res.data = resn;
            return res;
        }
    }
    setErrorneousResultAt(res); return res;
}
char isSymbolFirstCh(char ch){
    return (isalpha(ch)
    ||ch=='_'||ch=='-'||ch=='.'||ch=='='||ch=='+'
    ||ch=='*'||ch=='/'||ch=='&'||ch=='$'||ch=='%'
    ||ch=='^'||ch==':'||ch=='<'||ch=='>'||ch=='?'
    ||ch=='~'||ch=='`'
    );
}
char isSymbolCh(char ch){
    return (isSymbolFirstCh(ch)||isdigit(ch));
}
TokenizeResult isIdentifier(char* str){
    TokenizeResult res;
    Node* resn;
    char* idres;
    int i=0; int length = 0;
    if(!(isSymbolFirstCh(str[i]))){
        setErrorneousResultAt(res); return res;
    }
    else{
        i++; length++; col++;
        while(str[i]){
            if(isSymbolCh(str[i])){
                length++; i++; col++;
            }
            else break;
        }
        idres = malloc(length);
        memcpy(idres,str,length);
        res.type = VALID_TOKEN;
        resn = newNode;
        resn->type = IDENTIFIER;
        resn->data = (void*)idres;
        res.rest = str+length;
        res.data = resn;
        return res;
    }
}
/* yep i copied the definition of isIdentifier... */
TokenizeResult isBangIdentifier(char* str){
    TokenizeResult res;
    Node* resn;
    char* idres;
    int i=0; int length = 0;
    if(!str[i]||(!(str[i]=='!'))){
        setErrorneousResultAt(res); return res;
    }
    i++;length++;
    if(!(isSymbolFirstCh(str[i]))){
        setErrorneousResultAt(res); return res;
    }
    else{
        i++; length++; col++;
        while(str[i]){
            if(isSymbolCh(str[i])){
                length++; i++; col++;
            }
            else break;
        }
        idres = malloc(length);
        memcpy(idres,str,length);
        res.type = VALID_TOKEN;
        resn = newNode;
        resn->type = BANG_IDENTIFIER;
        resn->data = (void*)idres;
        res.rest = str+length;
        res.data = resn;
        return res;
    }
}
TokenizeResult isAtIdentifier(char* str){
    TokenizeResult res;
    Node* resn;
    char* idres;
    int i=0; int length = 0;
    if(!str[i]||(!(str[i]=='@'))){
        setErrorneousResultAt(res); return res;
    }
    i++;length++;
    if(!(isSymbolFirstCh(str[i]))){
        setErrorneousResultAt(res); return res;
    }
    else{
        i++; length++; col++;
        while(str[i]){
            if(isSymbolCh(str[i])){
                length++; i++; col++;
            }
            else break;
        }
        idres = malloc(length);
        memcpy(idres,str,length);
        res.type = VALID_TOKEN;
        resn = newNode;
        resn->type = AT_IDENTIFIER;
        resn->data = (void*)idres;
        res.rest = str+length;
        res.data = resn;
        return res;
    }
}

TokenizeResult isSymbol(char* str){
    TokenizeResult res;
    Node* resn;
    char* idres;
    int i=0; int length = 0;
    if(!str[i]||(!(str[i]=='#'))){
        setErrorneousResultAt(res); return res;
    }
    i++;length++;
    if(!isSymbolFirstCh(str[i])){
        setErrorneousResultAt(res); return res;
    }
    else{
        i++; length++; col++;
        while(str[i]){
            if(isSymbolCh(str[i])){
                length++; i++; col++;
            }
            else break;
        }
        idres = malloc(length);
        memcpy(idres,str,length);
        res.type = VALID_TOKEN;
        resn = newNode;
        resn->type = SYMBOL;
        resn->data = (void*)idres;
        res.rest = str+length;
        res.data = resn;
        return res;
    }
}
TokenizeResult isNumber(char* str){
    TokenizeResult res;
    Node* resn;
    double* floatres;
    long long* intres;
    int base=0, trail = 0, trailbase = 1, expbase = 0;
    int i=0;
    int sign, expbasesign;
    char hasfraction = 0;
    sign = (str[i]=='-'?-1:1);
    if(str[i]=='-'||str[i]=='+')i++,col++;
    if(str[i]&&!(isdigit(str[i])||str[i]=='.')){
        setErrorneousResultAt(res); return res;
    }
    while(str[i]&&isdigit(str[i])){
        base = base*10 + str[i]-'0';
        i++; col++;
    }
    if(str[i]=='.'){
        hasfraction = 1;
        i++; col++;
        while(str[i]&&isdigit(str[i])){
            trail = trail*10 + str[i]-'0';
            trailbase *= 10;
            i++; col++;
        }
    }
    if(str[i]=='e'||str[i]=='E'){
        i++; col++;
        expbasesign = (str[i]=='-'?-1:1);
        if(str[i]=='-'||str[i]=='+')i++,col++;
        /* TODO: change to fastexp
         * i used this method only because there's no pow func for long long
         * in c89. faster method should be used in the future...
         */
        while(str[i]&&isdigit(str[i])){
            expbase = expbase * 10 + str[i]-'0';
            i++; col++;
        }
    }
    res.type = VALID_TOKEN;
    resn = newNode;
    if(!hasfraction&&(expbase * expbasesign)>=0){
        intres = malloc(sizeof(long long));
        (*intres) = sign * base;
        while(expbase){(*intres)*=10,expbase--;}
        resn->type = INT_LITERAL;
        resn->data = (void*)intres;
    }else{
        floatres = malloc(sizeof(double));
        (*floatres) = sign * (base + (double)trail/trailbase)*pow(10,(expbasesign * expbase));
        resn->type = FLOAT_LITERAL;
        resn->data = (void*)floatres;
    }
    res.data = resn;
    res.rest = str+i;
    return res;
}

TokenizeResult isList_ (char* str, int layer){
    /*
     * "(branch #t (printstr "hell\n") (printstr "world\n"))"
     * 1. ( ---> isList ---> isList_ 1.
     * isList_ 1:
     * 1. prep NLN.
     * 2. branch ---> isIdentifier, cons `branch' resNLN
     * 3. #t ---> isBoolean, cons `TRUE' resNLN
     * 4. ( ---> isList ---> isList_ 2 =====>
     * isList_ 2:
     * 1. prep NLN.
     * 2. printstr ---> isIdentifier, cons `printstr' resNLN
     * 3. "hell\n" ---> isString, cons `"hell\n"' resNLN
     * 4. ) ---> return resNLN;
     * isList_ 1:
     * 5. cons resNLN2 resNLN
     */
    TokenizeResult fstres;
    NodeListNode* resNLN = NULL;
    NodeListNode* resNLNtail = NULL;
    NodeListNode* newNLN;
    Node* newN;
    char* stri = str;
    while(stri&&stri[0]){
        consumeWhitespace(&stri);
        /* 1. is it an identifier? */
        fstres = isIdentifier(stri);
        if(fstres.type == VALID_TOKEN){
            /* 2A. it is. */
            stri = fstres.rest;
            consumeWhitespace(&stri);
            newNLN = newNodeListNode;
            newNLN->next = NULL;
            newNLN->data = (void*)fstres.data;
            if(resNLNtail){
                resNLNtail->next = newNLN;
                resNLNtail = newNLN;
            }else{
                resNLN = resNLNtail = newNLN;
            }
            continue;
        }
        /* 2. is it a number? */
        fstres = isNumber(stri);
        if(fstres.type == VALID_TOKEN){
            /* 1A. it is. */
            stri = fstres.rest;
            consumeWhitespace(&stri);
            newNLN = newNodeListNode;
            newNLN->data = (void*)fstres.data;
            newNLN->next = NULL;
            if(resNLNtail){
                resNLNtail->next = newNLN;
                resNLNtail = newNLN;
            }else{
                resNLN = resNLNtail = newNLN;
            }
            continue;
        }
        /* 3-1. is it a bang-identifier? */
        fstres = isBangIdentifier(stri);
        if(fstres.type == VALID_TOKEN){
            /* 3A. it is. */
            stri = fstres.rest;
            consumeWhitespace(&stri);
            newNLN = newNodeListNode;
            newNLN->next = NULL;
            newNLN->data = (void*)fstres.data;
            if(resNLNtail){
                resNLNtail->next = newNLN;
                resNLNtail = newNLN;
            }else{
                resNLN = resNLNtail = newNLN;
            }
            continue;
        }
        /* 3-2. is it an at-identifier? */
        fstres = isAtIdentifier(stri);
        if(fstres.type == VALID_TOKEN){
            /* 3A. it is. */
            stri = fstres.rest;
            consumeWhitespace(&stri);
            newNLN = newNodeListNode;
            newNLN->next = NULL;
            newNLN->data = (void*)fstres.data;
            if(resNLNtail){
                resNLNtail->next = newNLN;
                resNLNtail = newNLN;
            }else{
                resNLN = resNLNtail = newNLN;
            }
            continue;
        }
        /* 3-3. is it an symbol? */
        fstres = isSymbol(stri);
        if(fstres.type == VALID_TOKEN){
            /* 3A. it is. */
            stri = fstres.rest;
            consumeWhitespace(&stri);
            newNLN = newNodeListNode;
            newNLN->next = NULL;
            newNLN->data = (void*)fstres.data;
            if(resNLNtail){
                resNLNtail->next = newNLN;
                resNLNtail = newNLN;
            }else{
                resNLN = resNLNtail = newNLN;
            }
            continue;
        }
        /* 4. is it a string literal? */
        fstres = isString(stri);
        if(fstres.type == VALID_TOKEN){
            /* 4A. it is. */
            stri = fstres.rest;
            consumeWhitespace(&stri);
            newNLN = newNodeListNode;
            newNLN->next = NULL;
            newNLN->data = (void*)fstres.data;
            if(resNLNtail){
                resNLNtail->next = newNLN;
                resNLNtail = newNLN;
            }else{
                resNLN = resNLNtail = newNLN;
            }
            continue;
        }
        /* 5. is it a list? */
        if(stri[0]=='('){
            col++;
            consumeWhitespace(&stri);
            fstres = isList_(stri+1,layer+1);
            if(fstres.type == VALID_TOKEN){
                /* 5A. yes it is. */
                /* now fstres is:
                 * VALID_TOKEN (LIST NodeList) reststr
                 * so...
                 */
                stri = fstres.rest;
                consumeWhitespace(&stri);
                newNLN = newNodeListNode;
                newNLN->next = NULL;
                newNLN->data = (void*)fstres.data;
                if(resNLNtail){
                    resNLNtail->next = newNLN;
                    resNLNtail = newNLN;
                }else{
                    resNLN = resNLNtail = newNLN;
                }
            }
            continue;
        }
        if(stri[0]==')'){
            /* 6. the list has ended. */
            fstres.type = VALID_TOKEN;
            newN = malloc(sizeof(Node));
            newN->type = LIST;
            newN->data = (void*)resNLN;
            fstres.data = (void*)newN;
            consumeWhitespace(&stri);
            fstres.rest = stri+1;
            col++;
            return fstres;
        }
        /* 7. not completed. */
        /* if there's no valid token even after the string has finished... */
        /* TODO: check if this is right
         * is this right? i'm not sure...
         */
        setErrorneousResultAt(fstres); return fstres;
    }
    /* 8. grammar error. */
    /* if everything's ok the procedure should return when it's
     * still in the loop above. so when it gets here there must
     * be something wrong with the input.
     */
    setErrorneousResultAt(fstres); return fstres;
}
TokenizeResult isList(char* str){
    /*
     * term     ::= boolean_literal | integer_literal | string_literal | identifier
     *            | "(" whitespaces termlist whitespaces ")"
     * termlist ::= term
     *            | term whitepaces termlist
     */
    TokenizeResult res;
    if(str[0]){
        if(str[0]!='('){
            setErrorneousResultAt(res); return res;
        }
        else {
            return isList_(str+1, 0);
        }
    } else {
        setErrorneousResultAt(res); return res;
    }
}

void printlist(NodeListNode* n,int layer);
void printsingl(Node* n,int layer){
    int i = 0;
    for(;i<layer;i++){putchar(' ');putchar(' ');}
    switch(n->type){
        case STRING_LITERAL:{
            printf("%s\n",(char*)(n->data));
            break;
        }
        case INT_LITERAL:{
            printf("%lld\n",(*(long long*)(n->data)));
            break;
        }
        case FLOAT_LITERAL:{
            printf("%g\n",(*(double*)(n->data)));
            break;
        }
        case IDENTIFIER:{
            printf("ID: %s\n",(char*)(n->data));
            break;
        }
        case BANG_IDENTIFIER:{
            printf("BANG: %s\n",(char*)(n->data));
            break;
        }
        case AT_IDENTIFIER:{
            printf("AT: %s\n",(char*)(n->data));
            break;
        }
        case SYMBOL:{
            printf("SYM: %s\n",(char*)(n->data));
            break;
        }
        case LIST:{
            printlist((NodeListNode*)(n->data),layer+1);
            break;
        }
    }
}
void printlist(NodeListNode* n,int layer){
    while(n){
        printsingl((Node*)(n->data),layer);
        n = n->next;
    }
}
/* uses isList([a string]); */

char buf[10000];
int main(){
    /* the driver. */
    
    TokenizeResult res;
    gets(buf);
    res = isList(buf);
    if(res.type == VALID_TOKEN){
        /* some testcases, for your interest.
         * the result should be right; the prettyprint proc is buggy, though.
         * (define (fac x) (if (<= x 1) 1 (* x (fac (- x 1)))))
         * (#f #t #void #F #T #VOID !BANG @AT)
         * (A (B (C (D (E) F) G) H) I)
         */
        printlist((NodeListNode*)(((Node*)(res.data))->data),0);
    } else {
        reportErrorWith(
            printf("BAHM!\n");
        );
    }
    return 0;
}

