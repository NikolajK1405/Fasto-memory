extern char* allocateHeap(int);
extern void printInt(int);

static char *hp;
static char *heap; // Nu ligger heap pointer ikke længere i x3/gp, er det et problem?
static int heapSize = 200;//100000;

// Type size should be 1 for bool and char or 4 for int or multi dimensional array
char *allocate(int n, int typeSize) { 
    if (heap == 0) { // If heap is not initialized, do so
        heap = allocateHeap(heapSize);
        hp = heap;
    }
    char *r = hp;
    hp += n * typeSize + 4;
    *((int*)r) = n;
    if (hp > heapSize + heap) {
        printInt(123); // Heap out of bounds, 
        // TODO find ud af korrekt kald til p.RuntimeError eller udvid heap
        return 0;
    }
    return r;
}
