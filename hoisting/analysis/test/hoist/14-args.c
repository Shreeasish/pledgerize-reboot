#include<stdio.h>

void
foo(int x, int y, int z) {
  printf("Hello %d %d %d", x, y, z);
}

int 
main() {
  int x = 7;
  int y = 5;
  int z = 10;
  foo(x, y, z);
}
