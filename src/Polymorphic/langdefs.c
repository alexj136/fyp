#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include <string.h>
#include <assert.h>
#include "langdefs.h"
#include "compiled.h"

/*
 * Allocate on the heap using malloc and assert that it was successful.
 */
void *ckMalloc(int size) {
    void *ptr = malloc(size);
    assert(ptr);
    return ptr;
}

/* 
 * Function that decides if two strings are equal. Returns 1 (true) if they are
 * the same, or 0 (false) if they differ.
 */
bool strEqual(char *str1, char *str2) {
	// If they have different lengths, we can say immediately that they differ
	if(strlen(str1) != strlen(str2)) return 0;
	// If they are she same length, we must use strncmp to compare them. strncmp
	// returns 0 for identical strings, and other ints for different ones, so we
	// negate the result.
	else return !strncmp(str1, str2, strlen(str1));
}

/*
 * Create new expression nodes of each type.
 */
Exp *newApp(Exp *fun, Exp *arg) {
    App *newAppNode = ckMalloc(sizeof(App));
    newAppNode->fun = fun;
    newAppNode->arg = arg;

    ExpV *val = ckMalloc(sizeof(ExpV));
    val->app = newAppNode;

    Exp *newExpNode = ckMalloc(sizeof(Exp));
    newExpNode->type = T_App;
    newExpNode->val = val;

    return newExpNode;
}

Exp *newAbs(Exp *body) {
    Abs *newAbsNode = ckMalloc(sizeof(Abs));
    newAbsNode->body = body;

    ExpV *val = ckMalloc(sizeof(ExpV));
    val->abs = newAbsNode;

    Exp *newExpNode = ckMalloc(sizeof(Exp));
    newExpNode->type = T_Abs;
    newExpNode->val = val;

    return newExpNode;
}

Exp *newVar(int bind) {
    Var *newVarNode = ckMalloc(sizeof(Var));
    newVarNode->bind = bind;

    ExpV *val = ckMalloc(sizeof(ExpV));
    val->var = newVarNode;

    Exp *newExpNode = ckMalloc(sizeof(Exp));
    newExpNode->type = T_Var;
    newExpNode->val = val;
    return newExpNode;
}

Exp *newCon(int conVal) {
    Con *newConNode = ckMalloc(sizeof(Con));
    newConNode->val = conVal;

    ExpV *val = ckMalloc(sizeof(ExpV));
    val->con = newConNode;

    Exp *newExpNode = ckMalloc(sizeof(Exp));
    newExpNode->type = T_Con;
    newExpNode->val = val;
    return newExpNode;
}

Exp *newOpn(OpTy type) {
    Opn *newOpnNode = ckMalloc(sizeof(Opn));
    newOpnNode->type = type;

    ExpV *val = ckMalloc(sizeof(ExpV));
    val->opn = newOpnNode;

    Exp *newExpNode = ckMalloc(sizeof(Exp));
    newExpNode->type = T_Opn;
    newExpNode->val = val;
    return newExpNode;
}

/*
 * Free an entire expression recursively.
 */
void freeExp(Exp *exp) {
    switch(exp->type) {
        case T_App:
            freeExp(exp->val->app->fun);
            freeExp(exp->val->app->arg);
            free(exp->val->app);
            free(exp->val);
            break;
        case T_Abs:
            freeExp(exp->val->abs->body);
            free(exp->val->abs);
            free(exp->val);
            break;
        case T_Var:
            free(exp->val->var);
            free(exp->val);
            break;
        case T_Con:
            free(exp->val->con);
            free(exp->val);
            break;
        case T_Opn:
            free(exp->val->opn);
            free(exp->val);
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
            return conVal(e1) == conVal(e2);
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
        printf("Error - unrecognised expression type\n");
        exit(EXIT_FAILURE);
    }
}

/*
 * Copy an expression, return a pointer to the newly allocated expression.
 */
Exp *copyExp(Exp *exp) {
    printf("copyExp() not yet implemented");
    exit(EXIT_FAILURE);
    return NULL;
}

/*
 * Member retrieval functions for expressions. Fail when an expression of the
 * wrong type is given as an argument.
 */
Exp *appFun(Exp *exp) {
    assert(exp->type == T_App);
    return exp->val->app->fun;
}

Exp *appArg(Exp *exp) {
    assert(exp->type == T_App);
    return exp->val->app->arg;
}

Exp *absBody(Exp *exp) {
    assert(exp->type == T_Abs);
    return exp->val->abs->body;
}

int varBind(Exp *exp) {
    assert(exp->type == T_Var);
    return exp->val->var->bind;
}

int conVal(Exp *exp) {
    assert(exp->type == T_Con);
    return exp->val->con->val;
}

OpTy opnType(Exp *exp) {
    assert(exp->type == T_Opn);
    return exp->val->opn->type;
}

/*
 * Reduce the indirectly referenced template. If any reduction was made, set the
 * value at the given boolean pointer to be false, indicating that the template
 * is not in normal form.
 */
void reduceTemplate(bool *normalForm, Exp **template) {

    Exp *exp = (*template);

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
    if(isApp(exp)
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
            (*template) = newCon(same);
            (*normalForm) = false;
        }
        if(!isCon(arg1)) {
            reduceTemplate(normalForm, &arg1);
        }
        else if(!isCon(arg2)) {
            reduceTemplate(normalForm, &arg2);
        }
        else {
            int arg1Val = conVal(arg1);
            int arg2Val = conVal(arg2);
            freeExp(exp);
            (*normalForm) = false;
                 if(opn == O_Add) { exp = newCon(arg1Val + arg2Val);        }
            else if(opn == O_Sub) { exp = newCon(arg1Val - arg2Val);        }
            else if(opn == O_Mul) { exp = newCon(arg1Val * arg2Val);        }
            else if(opn == O_Div) { exp = newCon(arg1Val / arg2Val);        }
            else if(opn == O_Mod) { exp = newCon(arg1Val % arg2Val);        }
            else if(opn == O_Lss) { exp = newCon(arg1Val < arg2Val);        }
            else if(opn == O_LsE) { exp = newCon(arg1Val <= arg2Val);       }
            else if(opn == O_NEq) { exp = newCon(arg1Val != arg2Val);       }
            else if(opn == O_Gtr) { exp = newCon(arg1Val > arg2Val);        }
            else if(opn == O_GtE) { exp = newCon(arg1Val >= arg2Val);       }
            else if(opn == O_Xor) { exp = newCon((!arg1Val) != (!arg2Val)); }
            else if(opn == O_And) { exp = newCon(arg1Val && arg2Val);       }
            else if(opn == O_Or ) { exp = newCon(arg1Val || arg2Val);       }
            else {
                printf("Error reducing binary operation - unrecognised "
                        "operation\n");
                exit(EXIT_FAILURE);
            }
        }
    }
    // End of binary operations case

    // iszero & not unary operations
    if(isApp(exp)
            && isOpn(appFun(exp))
            && (opnType(appFun(exp)) == O_Not
            || opnType(appFun(exp)) == O_IsZ)) {

        OpTy opn = opnType(appFun(exp));
        Exp *arg = appArg(exp);

        if(!isCon(arg)) {
            reduceTemplate(normalForm, &arg);
        }
        else {
            int argVal = conVal(arg);
            freeExp(exp);
            (*normalForm) = false;
            if           (opn == O_Not)  { exp = newCon(!argVal);     }
            else { assert(opn == O_IsZ);   exp = newCon(argVal == 0); }
        }
    }
    // End iszero & not unary operations case

    // Polymorphic unary operations
    // Null
    if(isApp(exp)
            && isOpn(appFun(exp))
            && (opnType(appFun(exp)) == O_Null)) {

        Exp *arg = appArg(exp);

        reduceTemplateNorm(&arg);
        if(isOpn(arg) && (opnType(arg) == O_Empty)) {
            freeExp(exp);
            exp = newCon(true);
            (*normalForm) = false;
        }
        else {
            freeExp(exp);
            exp = newCon(false);
            (*normalForm) = false;
        }
    }
    // End Null

    // Head and Tail
    if(isApp(exp)
            && isOpn(appFun(exp))
            && ((opnType(appFun(exp)) == O_Head)
            || (opnType(appFun(exp)) == O_Tail))) {

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
                exp = tmp;
                tmp = NULL;
                (*normalForm) = false;
            }
            else {
                assert(opn == O_Tail);
                Exp *tmp = copyExp(tail);
                freeExp(exp);
                exp = tmp;
                tmp = NULL;
                (*normalForm) = false;
            }
        }
    }
    // End Head and Tail

    // Cons
    if(isApp(exp)
            && isOpn(appFun(exp))
            && (opnType(appFun(exp)) == O_Cons)) {

        Exp *consArg = appArg(exp);

        reduceTemplate(normalForm, &consArg);
    }
    // End Cons
    
    // Sum operations
    if(isApp(exp)
            && isOpn(appFun(exp))
            && ((opnType(appFun(exp)) == O_RemL)
            || (opnType(appFun(exp)) == O_RemR))) {

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
                exp = tmp;
                tmp = NULL;
                (*normalForm) = false;
            }
            else {
                printf("Error - removed value from a non-sum expression or "
                        "wrong side of sum expression\n");
                exit(EXIT_FAILURE);
            }
        }
        else {
            reduceTemplate(normalForm, &arg);
        }
    }
    if(isApp(exp)
            && isOpn(appFun(exp))
            && ((opnType(appFun(exp)) == O_InjL)
            || (opnType(appFun(exp)) == O_InjR))) {

        Exp *arg = appArg(exp);

        reduceTemplate(normalForm, &arg);
    }
    // End sum operations
    
    // Tuple operations
    if(isApp(exp)
            && isOpn(appFun(exp))
            && ((opnType(appFun(exp)) == O_Fst)
            || (opnType(appFun(exp)) == O_Snd))) {

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
            exp = tmp;
            tmp = NULL;
            (*normalForm) = false;
        }
        else {
            reduceTemplate(normalForm, &arg);
        }
    }
    if(isApp(exp)
            && isOpn(appFun(exp))
            && (opnType(appFun(exp)) == O_Tuple)) {

        Exp *arg = appArg(exp);

        reduceTemplate(normalForm, &arg);
    }
    // End Tuple operations

    // Fixed point combinator
    if(isApp(exp)
            && isOpn(appFun(exp))
            && (opnType(appFun(exp)) == O_Fix)) {

        Exp *fCopy = copyExp(appArg(exp));

        exp = newApp(fCopy, exp);
    }
    // End fixed point combinator
    // End polymorphic unary operations
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
