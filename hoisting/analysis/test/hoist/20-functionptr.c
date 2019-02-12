#include<stdio.h>

int foo(int x) {
  if (x > 10) {
    printf("x > 10");
  }
  return 0;
}

int bar(int x) {
  if (x > 5) {
    printf("x > 5");
  }
  return 0;
}

int main() {
  int (*fp)(int);
  
  int y = 2;
  if (y > 1) {
    fp = &foo;
  } else {
    fp = &bar;
  }

  (fp)(12);
  return 0;
}
