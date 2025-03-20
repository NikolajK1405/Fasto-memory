#include "mem.h"
#include <assert.h>
#include <string.h>

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
