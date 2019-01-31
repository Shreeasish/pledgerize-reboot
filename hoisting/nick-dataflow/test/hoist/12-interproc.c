#include<stdio.h>

int
foo(int x) {
  if (x < 5) {
    return 30;
  }
  else {
    return 20;
  }
}

int
bar(int* x) {
  *x = *x + 25;
  return *x;
}

int
crash(int* x) {
  *x = *x + 60;
  return *x;
}

int main(int argc, char** argv) {
  int x = argv[0][0];

  bar(&x);

  x = foo(x);

  int z = crash(&x);

  x = z;

  if (x == 30) {
    printf("Value of x %d", z);
  }

  return 0;
}
