/*
 * Header file for langdefs.c.
 * Contains data definitions used by compiled programs.
 */
#ifndef LANGDEFS
#define LANGDEFS

typedef enum {
    false,
    true
} bool;

/*
 * Possible types of expression
 */
typedef enum {
    T_App,    // Function application
    T_Abs,    // Abstraction
    T_Var,    // Variable usage
    T_Con,    // Constant value e.g. 1, 'c', false
    T_Opn     // Operation e.g. +, head, xor
} ExpT;

/*
 * Possible expression values
 */
typedef union {
    struct App *app;
    struct Abs *abs;
    struct Var *var;
    struct Con *con;
    struct Opn *opn;
} ExpV;

/*
 * Exp represents an expression, carrying an ExpT so we know its type, and an
 * ExpV, an actual value.
 */
typedef struct Exp Exp;
struct Exp {
    ExpT type;
    ExpV *val;
};

/*
 * Function application - just pointers to the function and argument.
 */
typedef struct App App;
struct App {
    Exp *fun;
    Exp *arg;
};

/*
 * An abstraction stores its argument name, and the body expression.
 */
typedef struct Abs Abs;
struct Abs {
    char *name;
    Exp *body;
};


/*
 * A variable just carries its name.
 */
typedef struct Var Var;
struct Var {
    char *name;
};

/*
 * A constant can be an int, a char or a bool. We check this at compile-time, so
 * at run-time, we don't need to know which it is.
 */
typedef struct Con Con;
struct Con {
    int val;
};

/*
 * An operation just carries its type
 */
typedef struct Opn Opn;
struct Opn {
    OpTy type;
}

/*
 * The possible types of expression.
 */
typedef enum {
    O_Add; O_Sub; O_Mul; O_Div; O_Mod;  // Arithmetic

    O_Lss; O_LsE; O_NEq; O_Gtr; O_GtE;  // Integer Comparison

    O_Equ;                              // Expression equality

    O_And; O_Or ; O_Xor;                // Binary boolean operations

    O_Not; O_IsZ;                       // Unary operations

    O_Empty;                            // List operations
    O_Cons;
    O_Null;
    O_Head;
    O_Tail;

    O_Cond;                             // Conditionals

    O_Fix;                              // Fixed-point combinator

    O_InjL;                             // Sum-types
    O_Injr;
    O_RemL;
    O_RemR;

    O_Tuple;                            // Product types
    O_Fst;
    O_Snd
} OpTy;

/*
 * Allocate on the heap using malloc and assert that it was successful.
 */
void ckMalloc(int size);

/*
 * Functions to allocate & deallocate new expressions on the heap.
 */
Exp *newApp(Exp *fun, Exp *arg);
Exp *newAbs(char *name, Exp *body);
Exp *newVar(char *name);
Exp *newCon(int val);
Exp *newOpn(OpTy type);
void freeExp(Exp *exp);

#endif
