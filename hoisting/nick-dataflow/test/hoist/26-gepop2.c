#include<stdio.h>

struct {
  int x;
  int y;
  int z;
} global;

void modify(int* c) {
  *c += 7;
  return;
}

int main() {
  global.y = 2;
  global.y += 4;
  
  modify(&global.y);

  if (global.y + 3 > 2) {
    printf("Constant Expressions");
  }

  return 0;
}


