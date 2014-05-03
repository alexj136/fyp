#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include <string.h>
#include <assert.h>

#ifndef LANGDEFS
#include "langdefs.h"
#endif // LANGDEFS

#ifndef COMPILED
#include "compiled.h"
#endif // COMPILED

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

Exp *newAbs(char *name, Exp *body) {
    Abs *newAbsNode = ckMalloc(sizeof(Abs));
    newAbsNode->name = name;
    newAbsNode->body = body;

    ExpV *val = ckMalloc(sizeof(ExpV));
    val->abs = newAbsNode;

    Exp *newExpNode = ckMalloc(sizeof(Exp));
    newExpNode->type = T_Abs;
    newExpNode->val = val;

    return newExpNode;
}

Exp *newVar(char *name) {
    Var *newVarNode = ckMalloc(sizeof(Var));
    newVarNode->name = name;

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
            free(exp->val->abs->name);
            freeExp(exp->val->abs->body);
            free(exp->val->abs);
            free(exp->val);
            break;
        case T_Var:
            free(exp->val->var->name);
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
 * Determine whether or not two expressions are equal.
 */
bool expEqual(Exp *e1, Exp *e2) {
    printf("expEqual() not yet implemented");
    exit(EXIT_FAILURE);
}

/*
 * Copy an expression, return a pointer to the newly allocated expression.
 */
Exp *copyExp(Exp *exp) {
    printf("copyExp() not yet implemented");
    exit(EXIT_FAILURE);
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

char *absVar(Exp *exp) {
    assert(exp->type == T_Abs);
    return exp->val->abs->name;
}

Exp *absBody(Exp *exp) {
    assert(exp->type == T_Abs);
    return exp->val->abs->body;
}

char *varName(Exp *exp) {
    assert(exp->type == T_Var);
    return exp->val->var->name;
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
 * Reduce the template referenced by the given pointer. If it was found to have
 * normal form i.e. no reduction could be made, sets the dereferenced value at
 * normalForm to false.
 */
void reduceTemplate(bool *normalForm, Exp **template) {

    // Conditionals
    if(isApp(*template)) {
        Exp *falseExp = appArg(*template);
        Exp *t1 = appFun(*template);
        if(isApp(t1)) {
            Exp *trueExp = appArg(t1);
            Exp *t2 = appFun(t1);
            if(isApp(t2)) {
                Exp *guard = appArg(t2);
                Exp *cond = appFun(t2);
                if(isOpn(cond)) {
                    if(opnType(cond) == O_Cond) {
                        if(isCon(guard)) {
                            if(conVal(guard) == 0) {
                                Exp *temporary = copyExp(falseExp);
                                freeExp(*template);
                                (*template) = temporary;
                                temporary = NULL;
                                (*normalForm) = false;
                            }
                            else {
                                assert(conVal(guard) == 1);
                                Exp *temporary = copyExp(trueExp);
                                freeExp(*template);
                                (*template) = temporary;
                                temporary = NULL;
                                (*normalForm) = false;
                            }
                        }
                    }
                }
                else {
                    reduceTemplate(normalForm, &guard);
                }
            }
        }
    }
    // End of conditional case

    // Binary operations
    if(isApp(*template)) {
        Exp *arg2 = appArg(*template);
        Exp *t1 = appFun(*template);
        if(isApp(t1)) {
            Exp *arg1 = appArg(t1);
            Exp *opn = appFun(t1);
            if(isOpn(opn)) {
                if(opnType(opn) == O_Equ) {
                    reduceTemplateNorm(&arg1);
                    reduceTemplateNorm(&arg2);
                    bool same = expEqual(arg1, arg2);
                    freeExp(*template);
                    (*template) = newCon(same);
                    (*normalForm) = false;
                }
                else if(!isCon(arg1)) {
                    reduceTemplate(normalForm, &arg1);
                }
                else if(!isCon(arg2)) {
                    reduceTemplate(normalForm, &arg2);
                }
                else {
                    int arg1Val = conVal(arg1);
                    int arg2Val = conVal(arg2);
                    OpTy ty = opnType(opn);
                    freeExp(*template);
                    (*normalForm) = false;
                    switch(ty) {
                        case O_Add:
                            (*template) = newCon(arg1Val + arg2Val);
                            break;
                        case O_Sub:
                            (*template) = newCon(arg1Val - arg2Val);
                            break;
                        case O_Mul:
                            (*template) = newCon(arg1Val * arg2Val);
                            break;
                        case O_Div:
                            (*template) = newCon(arg1Val / arg2Val);
                            break;
                        case O_Mod:
                            (*template) = newCon(arg1Val % arg2Val);
                            break;
                        case O_Lss:
                            (*template) = newCon(arg1Val < arg2Val);
                            break;
                        case O_LsE:
                            (*template) = newCon(arg1Val <= arg2Val);
                            break;
                        case O_NEq:
                            (*template) = newCon(arg1Val != arg2Val);
                            break;
                        case O_Gtr:
                            (*template) = newCon(arg1Val > arg2Val);
                            break;
                        case O_GtE:
                            (*template) = newCon(arg1Val >= arg2Val);
                            break;
                        case O_Xor:
                            (*template) = newCon((!arg1Val) != (!arg2Val));
                            break;
                        case O_And:
                            (*template) = newCon(arg1Val && arg2Val);
                            break;
                        case O_Or:
                            (*template) = newCon(arg1Val || arg2Val);
                            break;
                    }
                }
            }
        }
    }
    // End of binary operations case

    // Unary operations
    if(isApp(*template)) {
        Exp *arg = appArg(*template);
        Exp *opn = appFun(*template);
        // Simple Cases - iszero and not
        if(isOpn(opn) && ((opnType(opn) == O_IsZ) || (opnType(opn) == O_Not))) {
            if(isCon(arg)) {
                OpTy ty = opnType(opn);
                int argVal = conVal(arg);
                freeExp(*template);
                switch(opnType(opn)) {
                    case O_IsZ:
                        (*template) = newCon(argVal == 0);
                        break;
                    case O_Not:
                        (*template) = newCon(!argVal);
                        break;
                }
            }
        }
        // More complicated cases - polymorphic operations
        else {
        }
    }

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
