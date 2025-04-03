#ifndef MEM_H
#define MEM_H

#ifndef DEBUG
    #define DEBUG 1  // Default to debug mode if not specified
#endif

// Only print in debug mode
#if DEBUG == 1
    #include <stdio.h>
    #include <stdlib.h>
    
    #define dprintf(...) printf(__VA_ARGS__)
    
    char *allocateHeap(int size);
    void freeHeap();
#else
    #define dprintf(...) ((void)0)
    
    extern char* allocateHeap(int);
    extern void printInt(int);
#endif
#include <stddef.h>
int *allocate(int n, int typeSize);
void deallocate(int *ptr);

#endif // MEM_H
