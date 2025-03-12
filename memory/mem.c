extern char* allocateHeap(int);
extern void printInt(int);

struct heapStruct
{
    int *freelist; // The freelist pointer points to a memory block, where first 4 bytes are block size. Thats why its a int pointer.
    char *heapStart;
    char *heapEnd;
    int heapSize;
};
static struct heapStruct hs = { 0, 0, 0, 1000 };
struct heapStruct *hsp = &hs;

// Type size should be 1 for bool and char or 4 for int or multi dimensional array
int *allocate(int n, int typeSize) { 
    if (hsp->heapStart == 0) { // If heap is not initialized, do so
        hsp->heapStart = allocateHeap(hsp->heapSize);
        hsp->heapEnd = hsp->heapStart + hsp->heapSize - 1;
        hsp->freelist = (int *)hsp->heapStart;
        *(hsp->freelist) = hsp->heapSize;
        *(hsp->freelist+4) = 0;
    }
    int size = n * typeSize + 8; // Make room for size header, and fasto size header
    size = (size + 3) & ~3;      // In case typeSize is 1, round up to be multiple of word size (4)
    int *prev = 0;
    int *curr = hsp->freelist; //Curr should be int pointer, since first 8 bytes of a block are two ints
    int currSize = 0;
    
    while (curr != 0) {
        currSize = *curr; 
        if (currSize > size + 8) {// if bigger than n+2*wordSize, split block
            *curr -= size;
            curr = curr + currSize - size; // Change current to lower part of block
            *curr = size;   // Set size field
            *(curr+4) = n;  // Change "next block pointer" field to fasto size field
            return curr+4;  // Return pointer to fasto size field
        } else if (currSize >= size) { // Perfect fit
            if (prev == 0) {  // Match is first block
                *hsp->freelist = *(curr+4); // Free list pointer points to the next block
            }
            else {
                *(prev+4) = *(curr+4);  // Previous pointer points to next block
            }
            *(curr+4) = n;  // Change "next block pointer" field to fasto size field
            return curr+4;  // Return pointer to fasto size field
        } else { // Get next block from free list
            prev = curr;
            curr = *(int **)(curr+4); // Load next block, since curr+4 is a pointer to a pointer we cast to char **
        }
    }
    return 0; // No space
}

// TODO, error handling
void deallocate(int *ptr) {
    *(int **)ptr = hsp->freelist; 
    hsp->freelist = ptr-4;
}
