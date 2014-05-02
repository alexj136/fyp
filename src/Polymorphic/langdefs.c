#include <stdio.h>
#include <malloc.h>
#include <string.h>
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

Exp *newCon(int val) {
    Con *newConNode = ckMalloc(sizeof(Con));
    newConNode->val = val;

    ExpV *val = ckMalloc(sizeof(ExpV));
    val->con = newConNode;

    Exp *newExpNode = ckMalloc(sizeof(Exp));
    newExpNode->type = T_Con;
    newExpNode->val = val;
    return newExpNode;
}

Exp *newOpn(OpTy type) {
    Opn *newConNode = ckMalloc(sizeof(Opn));
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
            Exp *arg2 = appArg(t1);
            Exp *opn = appFun(t1);
            if(isOpn(opn)) {
                switch(opnType(opn)) {
                    case O_Add:
                        // Do something
                        break;
                }
            }
        }
    }
    // End of binary operations case

}
