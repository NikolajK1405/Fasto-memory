#include "mem.h"

// Memory block
struct block {
    int size;           // Size of entire block
    struct block *next; // Pointer to next block, when allocated will contain fasto size (overloaded field)
    char data[]; 
};

struct heapStruct {   
    struct block *freelist; 
    char *heapStart;
    char *heapEnd;
    int heapSize;
};

static struct heapStruct hs = { 0, 0, 0, 1000 };
struct heapStruct *hsp = &hs;

#if DEBUG == 1
    char *allocateHeap(int size) {
        printf("Allocating heap...\n");
        
        char *heap = malloc(size * sizeof(char));
        if (heap == NULL) {
            printf("Malloc failed");
            return NULL;
        } else {return heap;}
    }
    void freeHeap() {
        free(hsp->heapStart);
    }
#endif

// Type size should be 1 for bool and char or 4 for int or multi dimensional array
int *allocate(int n, int typeSize) { 
    if (hsp->heapStart == 0) { // If heap is not initialized, do so
        hsp->heapStart = allocateHeap(hsp->heapSize);
        hsp->heapEnd = hsp->heapStart + hsp->heapSize - 1;
        struct block *bp = (struct block *)hsp->heapStart;
        bp->size = hsp->heapSize;
        bp->next = 0;
        hsp->freelist = bp;
    }
    int size = n * typeSize + 8; // Make room for size header, and fasto size header
    size = (size + 3) & ~3;      // In case typeSize is 1, round up to be multiple of word size (4)
    struct block *prev = 0;
    struct block *curr = hsp->freelist; // get first block in freelist
    int currSize = 0;
    
    while (curr != 0) {
        dprintf("Looking trough freelist...\n");
        currSize = curr->size; 
        dprintf("Current free block size: %d\n", currSize);
        if (currSize > size + 8) {// if bigger than n+2*wordSize, split block
            dprintf("Big block found, splitting block...\n");
            curr->size -= size;
            curr = (struct block *) (((char *)curr) + curr->size); // Change current to lower part of block, we cast to
            curr->size = size;   // Set size field                       byte pointer to perform byte size calulation and then back to int *
            curr->next = (struct block *)n;  // Change next field to fasto size field
            return (int *)(&curr->next);  // Return pointer to fasto size field
        } else if (currSize >= size) { // Perfect fit
            dprintf("Found perfect fit!\n");
            if (prev == 0) {  // Match is first block
                hsp->freelist = curr->next; // Free list pointer points to the next block
            }
            else {
                prev->next = curr->next;  // Previous pointer points to next block
            }
            curr->next = (struct block *)n;  // Change next field to fasto size field
            return (int *)(&curr->next);  // Return pointer to fasto size field
        } else { // Get next block from free list
            prev = curr;
            curr = curr->next; // Load next block, since curr+4 is a pointer to a pointer we cast to char **
        }
    }
    dprintf("No available space :(\n");
    return 0; // No space
}

// TODO, error handling
void deallocate(int *ptr) {
    // OBS! Using pointer arithmetic in RISC-V mode (DEBUG == 0), be careful if extending block structs.
    #if DEBUG == 1
        struct block *bp = (struct block *)(((char *)ptr) - offsetof(struct block, next));
    #else
        struct block *bp = (struct block *)(((char *)ptr) - 4); 
    #endif
    bp->next = hsp->freelist;
    hsp->freelist = bp;
}