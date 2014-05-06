#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include <assert.h>
#include "langdefs.h"

/*
 * Allocate on the heap using malloc and assert that it was successful.
 */
void *ckMalloc(int size) {
    void *ptr = malloc(size);
    assert(ptr);
    return ptr;
}

/*
 * Create new expression nodes of each type.
 */
Exp *newApp(Exp *fun, Exp *arg) {
    App *newAppNode = ckMalloc(sizeof(App));
    newAppNode->fun = fun;
    newAppNode->arg = arg;

    ExpV *valu = ckMalloc(sizeof(ExpV));
    valu->app = newAppNode;

    Exp *newExpNode = ckMalloc(sizeof(Exp));
    newExpNode->type = T_App;
    newExpNode->value = valu;

    return newExpNode;
}

Exp *newAbs(Exp *body) {
    Abs *newAbsNode = ckMalloc(sizeof(Abs));
    newAbsNode->body = body;

    ExpV *valu = ckMalloc(sizeof(ExpV));
    valu->abs = newAbsNode;

    Exp *newExpNode = ckMalloc(sizeof(Exp));
    newExpNode->type = T_Abs;
    newExpNode->value = valu;

    return newExpNode;
}

Exp *newVar(int bind) {
    Var *newVarNode = ckMalloc(sizeof(Var));
    newVarNode->bind = bind;

    ExpV *valu = ckMalloc(sizeof(ExpV));
    valu->var = newVarNode;

    Exp *newExpNode = ckMalloc(sizeof(Exp));
    newExpNode->type = T_Var;
    newExpNode->value = valu;
    return newExpNode;
}

Exp *newCon(ConTy ty, int conVal) {
    Con *newConNode = ckMalloc(sizeof(Con));
    newConNode->val = conVal;
    newConNode->ty = ty;

    ExpV *valu = ckMalloc(sizeof(ExpV));
    valu->con = newConNode;

    Exp *newExpNode = ckMalloc(sizeof(Exp));
    newExpNode->type = T_Con;
    newExpNode->value = valu;
    return newExpNode;
}

Exp *newOpn(OpTy type) {
    Opn *newOpnNode = ckMalloc(sizeof(Opn));
    newOpnNode->type = type;

    ExpV *valu = ckMalloc(sizeof(ExpV));
    valu->opn = newOpnNode;

    Exp *newExpNode = ckMalloc(sizeof(Exp));
    newExpNode->type = T_Opn;
    newExpNode->value = valu;
    return newExpNode;
}

/*
 * Free an entire expression recursively.
 */
void freeExp(Exp *exp) {
    switch(exp->type) {
        case T_App:
            freeExp(exp->value->app->fun);
            freeExp(exp->value->app->arg);
            free(exp->value->app);
            free(exp->value);
            break;
        case T_Abs:
            freeExp(exp->value->abs->body);
            free(exp->value->abs);
            free(exp->value);
            break;
        case T_Var:
            free(exp->value->var);
            free(exp->value);
            break;
        case T_Con:
            free(exp->value->con);
            free(exp->value);
            break;
        case T_Opn:
            free(exp->value->opn);
            free(exp->value);
            break;
    }
    free(exp);
}

/*
 * Functions to determine the type of an expression.
 */
bool isApp(Exp *exp) { return exp->type == T_App; }
bool isAbs(Exp *exp) { return exp->type == T_Abs; }
bool isVar(Exp *exp) { return exp->type == T_Var; }
bool isCon(Exp *exp) { return exp->type == T_Con; }
bool isOpn(Exp *exp) { return exp->type == T_Opn; }

/*
 * Determine if an operation is a binary operation.
 */
bool isBinaryOpn(Exp *exp) {
    if(isOpn(exp)) {
        OpTy opn = opnType(exp);
        return (opn == O_Add) || (opn == O_Sub) || (opn == O_Mul)
            || (opn == O_Div) || (opn == O_Mod) || (opn == O_Lss)
            || (opn == O_LsE) || (opn == O_NEq) || (opn == O_Gtr)
            || (opn == O_GtE) || (opn == O_And) || (opn == O_Or )
            || (opn == O_Xor) || (opn == O_Equ);
    }
    else {
        return false;
    }
}

/*
 * Determine whether or not two expressions are equal.
 */
bool expEqual(Exp *e1, Exp *e2) {
    if(isApp(e1)) {
        if(!isApp(e2)) {
            return false;
        }
        else {
            return expEqual(appFun(e1), appFun(e2))
                && expEqual(appArg(e1), appArg(e2));
        }
    }
    else if(isAbs(e1)) {
        if(!isAbs(e2)) {
            return false;
        }
        else {
            return expEqual(absBody(e1), absBody(e2));
        }
    }
    else if(isVar(e1)) {
        if(!isVar(e2)) {
            return false;
        }
        else {
            return varBind(e1) == varBind(e2);
        }
    }
    else if(isCon(e1)) {
        if(!isCon(e2)) {
            return false;
        }
        else {
            return (conVal(e1) == conVal(e2)) && (conTy(e1) == conTy(e2));
        }
    }
    else if(isOpn(e1)) {
        if(!isOpn(e2)) {
            return false;
        }
        else {
            return opnType(e1) == opnType(e2);
        }
    }
    else {
        printf("Error - unrecognised expression type in expEqual()\n");
        assert(false);
    }
}

/*
 * Copy an expression, return a pointer to the newly allocated expression.
 */
Exp *copyExp(Exp *exp) {
    if(isApp(exp)) {
        return newApp(copyExp(appFun(exp)), copyExp(appArg(exp)));
    }
    else if(isAbs(exp)) {
        return newAbs(copyExp(absBody(exp)));
    }
    else if(isVar(exp)) {
        return newVar(varBind(exp));
    }
    else if(isCon(exp)) {
        return newCon(conTy(exp), conVal(exp));
    }
    else if(isOpn(exp)) {
        return newOpn(opnType(exp));
    }
    else {
        printf("Error - unrecognised expression type in copyExp()\n");
        assert(false);
    }
}

/*
 * Print an expression to stdout, folled by a newline character.
 */
void printlnExp(Exp *exp) {
    printExp(exp);
    printf("\n");
}

/*
 * Print an expression to stdout.
 */
void printExp(Exp *exp) {
    if(isApp(exp)) {
        printf("(");
        printExp(appFun(exp));
        printf(" ");
        printExp(appArg(exp));
        printf(")");
    }
    else if(isAbs(exp)) {
        printf("(\\ ");
        printExp(absBody(exp));
        printf(")");
    }
    else if(isVar(exp)) {
        if(varBind(exp) > 0) {
            printf("V%d", varBind(exp));
        }
        else {
            printf("F%d", -varBind(exp));
        }
    }
    else if(isCon(exp) && (conTy(exp) == C_Bool) && (conVal(exp) == true)) {
        printf("true");
    }
    else if(isCon(exp) && (conTy(exp) == C_Bool) && (conVal(exp) == false)) {
        printf("false");
    }
    else if(isCon(exp) && (conTy(exp) == C_Char)) {
        printf("\'%c\'", conVal(exp));
    }
    else if(isCon(exp)/* && (conTy(exp) == C_Int)*/) {
        printf("%d", conVal(exp));
    }
    else if(isOpn(exp)) {
        switch(opnType(exp)) {
            case O_Cond : printf("cond")  ; break;
            case O_Add  : printf("+")     ; break;
            case O_Sub  : printf("-")     ; break;
            case O_Mul  : printf("*")     ; break;
            case O_Div  : printf("/")     ; break;
            case O_Mod  : printf("%%")    ; break;
            case O_Lss  : printf("<")     ; break;
            case O_LsE  : printf("<=")    ; break;
            case O_NEq  : printf("/=")    ; break;
            case O_Gtr  : printf(">")     ; break;
            case O_GtE  : printf(">=")    ; break;
            case O_Equ  : printf("==")    ; break;
            case O_And  : printf("and")   ; break;
            case O_Or   : printf("or")    ; break;
            case O_Xor  : printf("xor")   ; break;
            case O_Not  : printf("not")   ; break;
            case O_IsZ  : printf("iszero"); break;
            case O_Empty: printf("[]")    ; break;
            case O_Cons : printf(":")     ; break;
            case O_Null : printf("null")  ; break;
            case O_Head : printf("head")  ; break;
            case O_Tail : printf("tail")  ; break;
            case O_Fix  : printf("fix")   ; break;
            case O_InjL : printf("injl")  ; break;
            case O_InjR : printf("injr")  ; break;
            case O_RemL : printf("reml")  ; break;
            case O_RemR : printf("remr")  ; break;
            case O_Tuple: printf("tuple") ; break;
            case O_Fst  : printf("fst")   ; break;
            case O_Snd  : printf("snd")   ; break;
        }
    }
    else {
        printf("Error - unrecognised expression type in printExp()\n");
        assert(false);
    }
}

/*
 * Member retrieval functions for expressions. Fail when an expression of the
 * wrong type is given as an argument.
 */
Exp *appFun(Exp *exp) {
    assert(exp->type == T_App);
    return exp->value->app->fun;
}

Exp *appArg(Exp *exp) {
    assert(exp->type == T_App);
    return exp->value->app->arg;
}

Exp *absBody(Exp *exp) {
    assert(exp->type == T_Abs);
    return exp->value->abs->body;
}

int varBind(Exp *exp) {
    assert(exp->type == T_Var);
    return exp->value->var->bind;
}

ConTy conTy(Exp *exp) {
    assert(exp->type == T_Con);
    return exp->value->con->ty;
}

int conVal(Exp *exp) {
    assert(exp->type == T_Con);
    return exp->value->con->val;
}

OpTy opnType(Exp *exp) {
    assert(exp->type == T_Opn);
    return exp->value->opn->type;
}

/*
 * Reduce the indirectly referenced template. If any reduction was made, set the
 * value at the given boolean pointer to be false, indicating that the template
 * is not in normal form.
 */
void reduceTemplate(bool *normalForm, Exp **template) {

    Exp *exp = (*template);
    printf("!! "); printlnExp(exp);

    // Conditionals
    if(isApp(exp)
            && isApp(appFun(exp))
            && isApp(appFun(appFun(exp)))
            && isOpn(appFun(appFun(appFun(exp))))
            && (opnType(appFun(appFun(appFun(exp)))) == O_Cond)) {

        Exp *guard  = appArg(appFun(appFun(exp)));
        Exp *truExp = appArg(appFun(exp));
        Exp *falExp = appArg(exp);

        // If the guard is true, replace the expression with the true argument.
        if(isCon(guard) && (conVal(guard) == true)) {
            Exp *tmp = copyExp(truExp);
            freeExp(exp);
            (*template) = tmp;
            tmp = NULL;
            (*normalForm) = false;
        }
        // If the guard is false, replace the expression with the false
        // argument.
        else if(isCon(guard) && (conVal(guard) == false)) {
            Exp *tmp = copyExp(falExp);
            freeExp(exp);
            (*template) = tmp;
            tmp = NULL;
            (*normalForm) = false;
        }
        // If the guard is not reduced, reduce it.
        else {
            reduceTemplate(normalForm, &guard);
        }
    }
    // End of conditional case

    // Binary operations
    else if(isApp(exp)
            && isApp(appFun(exp))
            && isBinaryOpn(appFun(appFun(exp)))) {

        OpTy opn  = opnType(appFun(appFun(exp)));
        Exp *arg1 = appArg(appFun(exp));
        Exp *arg2 = appArg(exp);

        // Handle equality differently because it is polymorphic.
        if(opn == O_Equ) {
            reduceTemplateNorm(&arg1);
            reduceTemplateNorm(&arg2);
            bool same = expEqual(arg1, arg2);
            freeExp(exp);
            (*template) = newCon(C_Bool, same);
            (*normalForm) = false;
        }
        else if(isApp(arg1) || isAbs(arg1) || isVar(arg1) || isOpn(arg1)) {
            printf("reducing "); printExp(arg1); printf("\n");
            reduceTemplate(normalForm, &arg1);
            printf("done\n");
        }
        else if(isApp(arg2) || isAbs(arg2) || isVar(arg2) || isOpn(arg2)) {
            printf("reducing "); printExp(arg2); printf("\n");
            reduceTemplate(normalForm, &arg2);
            printf("done\n");
        }
        else {
            assert(isCon(arg1) && isCon(arg2));
            int arg1Val = conVal(arg1);
            int arg2Val = conVal(arg2);
            freeExp(exp);
            (*normalForm) = false;
            if(opn == O_Add) {
                printf("doing %d + %d\n", arg1Val, arg2Val);
                (*template) = newCon(C_Int, arg1Val + arg2Val);
                printf("got "); printlnExp(*template);
            }
            else if(opn == O_Sub) {
                (*template) = newCon(C_Int, arg1Val - arg2Val);
            }
            else if(opn == O_Mul) {
                (*template) = newCon(C_Int, arg1Val * arg2Val);
            }
            else if(opn == O_Div) {
                (*template) = newCon(C_Int, arg1Val / arg2Val);
            }
            else if(opn == O_Mod) {
                (*template) = newCon(C_Int, arg1Val % arg2Val);
            }
            else if(opn == O_Lss) {
                (*template) = newCon(C_Bool, arg1Val < arg2Val);
            }
            else if(opn == O_LsE) {
                (*template) = newCon(C_Bool, arg1Val <= arg2Val);
            }
            else if(opn == O_NEq) {
                (*template) = newCon(C_Bool, arg1Val != arg2Val);
            }
            else if(opn == O_Gtr) {
                (*template) = newCon(C_Bool, arg1Val > arg2Val);
            }
            else if(opn == O_GtE) {
                (*template) = newCon(C_Bool, arg1Val >= arg2Val);
            }
            else if(opn == O_Xor) {
                (*template) = newCon(C_Bool, (!arg1Val) != (!arg2Val));
            }
            else if(opn == O_And) {
                (*template) = newCon(C_Bool, arg1Val && arg2Val);
            }
            else if(opn == O_Or ) {
                (*template) = newCon(C_Bool, arg1Val || arg2Val);
            }
            else {
                printf("Error reducing binary operation - unrecognised "
                        "operation\n");
                assert(false);
            }
        }
    }
    // End of binary operations case

    // iszero & not unary operations
    else if(isApp(exp)
            && isOpn(appFun(exp))
            && (opnType(appFun(exp)) == O_Not
            || opnType(appFun(exp)) == O_IsZ)) {
        printf("app unop _\n");

        OpTy opn = opnType(appFun(exp));
        Exp *arg = appArg(exp);

        if(isApp(arg) || isAbs(arg) || isVar(arg) || isOpn(arg)) {
            reduceTemplate(normalForm, &arg);
        }
        else {
            assert(isCon(arg));
            int argVal = conVal(arg);
            freeExp(exp);
            (*normalForm) = false;
            if(opn == O_Not)  {
                (*template) = newCon(C_Bool, !argVal);
            }
            else {
                assert(opn == O_IsZ);
                (*template) = newCon(C_Bool, argVal == 0);
            }
        }
    }
    // End iszero & not unary operations case

    // Polymorphic unary operations
    // Null
    else if(isApp(exp)
            && isOpn(appFun(exp))
            && (opnType(appFun(exp)) == O_Null)) {
        printf("app null _\n");

        Exp *arg = appArg(exp);

        reduceTemplateNorm(&arg);
        if(isOpn(arg) && (opnType(arg) == O_Empty)) {
            freeExp(exp);
            (*template) = newCon(C_Bool, true);
            (*normalForm) = false;
        }
        else {
            freeExp(exp);
            (*template) = newCon(C_Bool, false);
            (*normalForm) = false;
        }
    }
    // End Null

    // Head and Tail
    else if(isApp(exp)
            && isOpn(appFun(exp))
            && ((opnType(appFun(exp)) == O_Head)
            || (opnType(appFun(exp)) == O_Tail))) {
        printf("app [head|tail] _\n");

        OpTy opn = opnType(appFun(exp));
        Exp *arg = appArg(exp);

        if(isApp(arg)
                && isApp(appFun(arg))
                && isOpn(appFun(appFun(arg)))
                && (opnType(appFun(appFun(arg)))) == O_Cons) {
            
            Exp *head = appArg(appFun(arg));
            Exp *tail = appArg(arg);

            if(opn == O_Head) {
                Exp *tmp = copyExp(head);
                freeExp(exp);
                (*template) = tmp;
                tmp = NULL;
                (*normalForm) = false;
            }
            else {
                assert(opn == O_Tail);
                Exp *tmp = copyExp(tail);
                freeExp(exp);
                (*template) = tmp;
                tmp = NULL;
                (*normalForm) = false;
            }
        }
        else {
            reduceTemplate(normalForm, &arg);
        }
    }
    // End Head and Tail

    // Cons
    else if(isApp(exp)
            && isOpn(appFun(exp))
            && (opnType(appFun(exp)) == O_Cons)) {
        printf("app cons _\n");

        Exp *consArg = appArg(exp);

        reduceTemplate(normalForm, &consArg);
    }
    // End Cons
    
    // Sum operations
    else if(isApp(exp)
            && isOpn(appFun(exp))
            && ((opnType(appFun(exp)) == O_RemL)
            || (opnType(appFun(exp)) == O_RemR))) {
        printf("app [reml|remr] _\n");

        OpTy opn = opnType(appFun(exp));
        Exp *arg = appArg(exp);

        if(isApp(arg)
                && isOpn(appFun(arg))
                && ((opnType(appFun(arg)) == O_InjL)
                || (opnType(appFun(arg)) == O_InjR))) {

            OpTy innerOpn = opnType(appFun(arg));
            Exp *innerArg = appArg(arg);

            if(((opn == O_RemL) && (innerOpn == O_InjL))
                    || ((opn == O_RemR) && (innerOpn == O_InjR))) {
                
                Exp *tmp = copyExp(innerArg);
                freeExp(exp);
                (*template) = tmp;
                tmp = NULL;
                (*normalForm) = false;
            }
            else {
                printf("Error - removed value from a non-sum expression or "
                        "wrong side of sum expression\n");
                assert(false);
            }
        }
        else {
            reduceTemplate(normalForm, &arg);
        }
    }
    else if(isApp(exp)
            && isOpn(appFun(exp))
            && ((opnType(appFun(exp)) == O_InjL)
            || (opnType(appFun(exp)) == O_InjR))) {
        printf("app [injl|injr] _\n");

        Exp *arg = appArg(exp);

        reduceTemplate(normalForm, &arg);
    }
    // End sum operations
    
    // Tuple operations
    else if(isApp(exp)
            && isOpn(appFun(exp))
            && ((opnType(appFun(exp)) == O_Fst)
            || (opnType(appFun(exp)) == O_Snd))) {
        printf("app [fst|snd] _\n");

        OpTy opn = opnType(appFun(exp));
        Exp *arg = appArg(exp);

        if(isApp(arg)
                && isApp(appFun(arg))
                && isOpn(appFun(appFun(arg)))
                && (opnType(appFun(appFun(arg))) == O_Tuple)) {

            Exp *fst = appArg(appFun(arg));
            Exp *snd = appArg(arg);

            Exp *tmp = copyExp((opn == O_Fst) ? fst : snd);
            freeExp(exp);
            (*template) = tmp;
            tmp = NULL;
            (*normalForm) = false;
        }
        else {
            reduceTemplate(normalForm, &arg);
        }
    }
    else if(isApp(exp)
            && isOpn(appFun(exp))
            && (opnType(appFun(exp)) == O_Tuple)) {
        printf("app tuple _\n");

        Exp *arg = appArg(exp);

        reduceTemplate(normalForm, &arg);
    }
    // End Tuple operations

    // Fixed point combinator
    else if(isApp(exp)
            && isOpn(appFun(exp))
            && (opnType(appFun(exp)) == O_Fix)) {
        printf("app fix _\n");

        Exp *fCopy = copyExp(appArg(exp));

        (*template) = newApp(fCopy, exp);
        (*normalForm) = false;
    }
    // End fixed point combinator
    // End polymorphic unary operations

    // Lambda abstractions
    else if(isApp(exp)
            && isAbs(appFun(exp))) {
        printf("app (abs _) _\n");

        Exp *abs = appFun(exp);
        Exp *arg = appArg(exp);

        Exp *tmp = replace(absBody(abs), 0, arg);
        freeExp(exp);
        (*template) = tmp;
        tmp = NULL;
        (*normalForm) = false;
    }
    // End lambda abstractions

    // Function calls
    else if(isApp(exp)
            && isVar(appFun(exp))) {
        printf("app (var _) _\n");

        Exp *var = appFun(exp);
        Exp *arg = appArg(exp);

        if(hasFunc(varBind(var))) {

            int bind = varBind(var);
            freeExp(var);
            exp->value->app->fun = instantiate(bind);
            (*normalForm) = false;
        }
        else {
            reduceTemplate(normalForm, &arg);
        }
    }
    else if(isVar(exp)
            && hasFunc(varBind(exp))) {
        printf("var _\n");

        int bind = varBind(exp);
        freeExp(exp);
        (*template) = instantiate(bind);
        (*normalForm) = false;
    }
    // End function calls

    // Catch-all application case
    else if(isApp(exp)) {
        printf("app _ _\n");
        Exp *fun = appFun(exp);
        Exp *arg = appArg(exp);
        reduceTemplate(normalForm, &fun);
        reduceTemplate(normalForm, &arg);
    }
    // End catch-all application case
}

/*
 * Perform reduction on a template until it reaches its normal form (when no
 * reduction rules are applicable).
 */
void reduceTemplateNorm(Exp **template) {
    bool normalForm = false;
    while(!normalForm) {
        normalForm = true;
        reduceTemplate(&normalForm, template);
    }
}

/*
 * Copy an expression, but replace all occurences of a given variable with a
 * given expression.
 */
Exp *replace(Exp *body, int bind, Exp *arg) { // :-) YOU CAN DO IT!!!
    if(isApp(body)) {
        return newApp(replace(appFun(body), bind, arg),
                replace(appArg(body), bind, arg));
    }
    else if(isAbs(body)) {
        return newAbs(replace(absBody(body), bind + 1, arg));
    }
    else if(isVar(body)) {
        if(varBind(body) == bind) {
            return copyExp(arg);
        }
        else {
            return copyExp(body);
        }
    }
    else if(isCon(body) || isOpn(body)) {
        return copyExp(body);
    }
    else {
        printf("Error - unrecognised expression type in replace()\n");
        assert(false);
        return NULL;
    }
}
