#include <stdio.h>
#include <malloc.h>
#include "langdefs.h"

/*
 * Allocate on the heap using malloc and assert that it was successful.
 */
void *ckMalloc(int size) {
    void *ptr = malloc(size);
    assert(ptr);
    return ptr;
}

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
