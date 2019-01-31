#include<stdio.h>

void foo(int *x) {
  *x = *x + 15;
}


int main() {
  int x = 10;
  foo(&x);

  if (x > 15) {
    printf("blah");
  }

  return 0;
}
