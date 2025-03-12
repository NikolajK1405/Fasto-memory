#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>

struct heapStruct
{   // The freelist pointer points to a memory block, where first 4 bytes are block size. Thats why its a int pointer.
    // It should be noted that the second field is overloaded. When the block is free it contains pointer to the next block,
    // however when allocated, it will contain the "Fasto size" following the fasto conventions.
    int *freelist; 
    char *heapStart;
    char *heapEnd;
    int heapSize;
};
static struct heapStruct hs = { 0, 0, 0, 1000 };
struct heapStruct *hsp = &hs;

char *allocateHeap(int size) {
    printf("Allocating heap...\n");
    
    char *heap = malloc(size * sizeof(char));
    if (heap == NULL) {
        printf("Malloc failed");
        return NULL;
    } else {return heap;}
}

// Type size should be 1 for bool and char or 4 for int or multi dimensional array
int *allocate(int n, int typeSize) { 
    if (hsp->heapStart == 0) { // If heap is not initialized, do so
        hsp->heapStart = allocateHeap(hsp->heapSize);
        hsp->heapEnd = hsp->heapStart + hsp->heapSize - 1;
        hsp->freelist = (int *)hsp->heapStart;
        *(hsp->freelist) = hsp->heapSize;
        *(hsp->freelist+1) = 0;
    }
    int size = n * typeSize + 8; // Make room for size header, and fasto size header
    size = (size + 3) & ~3;      // In case typeSize is 1, round up to be multiple of word size (4)
    int *prev = 0;
    int *curr = hsp->freelist; //Curr should be int pointer, since first 8 bytes of a block are two ints
    int currSize = 0;
    
    while (curr != 0) {
        printf("Looking trough freelist...\n");
        currSize = *curr; 
        printf("Current free block size: %d\n", currSize);
        if (currSize > size + 8) {// if bigger than n+2*wordSize, split block
            printf("Big block found, splitting block...\n");
            *curr -= size;
            curr = (int *) ((char *) curr + (currSize - size)); // Change current to lower part of block, we cast to
            *curr = size;   // Set size field                       byte pointer to perform byte size calulation and then back to int *
            *(curr+1) = n;  // Change "next block pointer" field to fasto size field
            return curr+1;  // Return pointer to fasto size field
        } else if (currSize >= size) { // Perfect fit
            printf("Found perfect fit!\n");
            if (prev == 0) {  // Match is first block
                hsp->freelist = *(int **)(curr+1); // Free list pointer points to the next block
            }
            else {
                *(prev+1) = *(curr+1);  // Previous pointer points to next block
            }
            *(curr+1) = n;  // Change "next block pointer" field to fasto size field
            return curr+1;  // Return pointer to fasto size field
        } else { // Get next block from free list
            prev = curr;
            curr = *(int **)(curr+1); // Load next block, since curr+4 is a pointer to a pointer we cast to char **
        }
    }
    printf("No available space :(\n");
    return 0; // No space
}

// TODO, error handling
void deallocate(int *ptr) {
    *(int **)ptr = hsp->freelist; 
    hsp->freelist = ptr-1;
}

void freeHeap() {
    free(hsp->heapStart);
}

int main(void) {

    printf("Running allocator tests...\n");
    
    // 1) Test allocation and reading of 10 bools
    {
        int size = 10;
        int *p = allocate(size, 1);     // allocate space for 10 bools
        assert(p != NULL);              // verify allocation succeeded
        assert(*p == size);
        char *data = (char *)(p + 1);   // Get past size field

        for (int i = 0; i < size; i++) {
            data[i] = i % 2;
        }

        for (int i = 0; i < size; i++) {
            assert(data[i] == (i % 2));
        }

        // Cleanup
        deallocate(p);
        printf("Test 1 (bool array) passed.\n");
    }

    // 2) Test allocation and reading of a string, char array
    {
        int size = 5;
        int *p = allocate(size, 1);     // allocate space for 5 char
        assert(p != NULL);              // verify allocation succeeded
        assert(*p == size);
        char *data = (char *)(p + 1);   // Get past size field

        strcpy(data, "Fasto");
        assert(strcmp(data, "Fasto") == 0);

        // Cleanup
        deallocate(p);
        printf("Test 2 (string) passed.\n");
    }

    // 3) Test allocation and reading of int array
    {
        int size = 5;
        int *p = allocate(size, 4);     // allocate space for 5 ints
        assert(p != NULL);              // verify allocation succeeded
        assert(*p == size);
        int *data = p + 1;    // Get past size field, no need to cast to char* as we are storing ints

        for (int i = 0; i < size; i++) {
            data[i] = i * 10;
        }
        
        for (int i = 0; i < size; i++) {
            assert(data[i] == i * 10);
        }

        // Cleanup
        deallocate(p);
        printf("Test 3 (int array) passed.\n");
    }

    // 4) Test multiple allocations before deallocation
    {
        int *p1 = allocate(3, 1);
        char *data1 = (char *)(p1 + 1);

        int *p2 = allocate(3, 1);
        char *data2 = (char *)(p2 + 1); 

        int *p3 = allocate(3, 1);
        char *data3 = (char *)(p3 + 1); 

        assert(p1 != NULL && p2 != NULL && p3 != NULL);

        strcpy(data1, "One");
        strcpy(data2, "Two");
        strcpy(data3, "Tre");

        // Confirm
        assert(strcmp(data1, "One") == 0);
        assert(strcmp(data2, "Two") == 0);
        assert(strcmp(data3, "Tre") == 0);

        // Deallocate in a different order
        deallocate(p2);
        deallocate(p1);
        deallocate(p3);

        printf("Test 4 (multiple allocations) passed.\n");
    }

    // 5) Attempt an allocation that will exceed the heap size
    {
        int *p = allocate(2000, 1); 
        assert(p == NULL);

        printf("Test 5 (over-allocation) passed.\n");
    }

    printf("All tests passed successfully.\n");
    freeHeap();
    return 0;
}
