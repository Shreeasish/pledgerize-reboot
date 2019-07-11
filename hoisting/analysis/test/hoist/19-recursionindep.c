#include<stdio.h>
void foo(int);
void bar(int);

void
bar(int flag) {
  foo(flag);
  return;
}

void
foo(int flag) {
  if (flag > 5) {
    printf("hello");
    bar(flag);
  }
}

int
main() {
  foo(30);
}
