extern void printInt(int);

// Recursive approach
int fibRec(int x) {
  if (x <= 0) return 0;
  if (x == 1) return 1;
  
  return fibRec(x - 1) + fibRec(x - 2);
}

// Iterative approach
int fibIter(int x) {
  if (x <= 0) return 0;
  if (x == 1) return 1;
    
  int a = 0, b = 1, c;
  for (int i = 2; i <= x; i++) {
    c = a + b;
    a = b;
    b = c;
  }
  return b;
}

int main() {
  int x = 25;
    
  printInt(fibRec(x));
  printInt(fibIter(x));
    
  return 0;
}


