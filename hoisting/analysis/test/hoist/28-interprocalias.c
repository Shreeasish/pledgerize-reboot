#include<stdio.h>

struct {
  int a;
  short int b[1];
  int c;
} global;

void
check(int* z, int* y) {
  if (*y == *z) {
    printf("the same");
  }
}

void
foo(short int* x) {
  *x = 2;
}

void
bar(int* x, int* y) {
  *x = 5;
  *y = 6;
}

int main() {
  int x = 0;

  bar(&x,&x);
  foo(&global.b[1]);
  check(&x, &x);

  return 0;
}
