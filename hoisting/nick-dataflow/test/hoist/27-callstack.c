#include<stdio.h>

void func(int z) {
  if (z > 2) {
    printf("make this work from inside a function");
  }
}

int main() {
  int x = 0;

  func(x);
  if (x > 2) {
    printf("Make this work");
  }

  return 0;
}
