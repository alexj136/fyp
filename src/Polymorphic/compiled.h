/*
 * Header file for compiled code - allows code in langdefs.c to make calls to
 * compiled code without having to generate a langdefs.c & langdefs.h file each
 * time the compiler is run.
 */

#ifndef COMPILED
#define COMPILED

#ifndef LANGDEFS
#include "langdefs.h"
#endif // LANGDEFS

/*
 * Instantiate on the heap a template for the function with the given index
 * number, and return a pointer to it.
 */
Exp *instantiate(int funcNo);

/*
 * Determine if the function with given index number exists
 */
bool hasFunc(int funcNo);

#endif // COMPILED
