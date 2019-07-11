#include<stdio.h>

int main() {
  int x = 10;
  int* y = &x;
  if (*y == 10) {
    printf("dun dun dun");
  }
}
