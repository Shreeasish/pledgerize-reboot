#include<stdio.h>

int func(int* x) {
  
}

int main() {
  int* ptrVec[10];
  int x = 30;
  int i;

  for (i = 0; i < 10; i++) {
    ptrVec[i] = &x;
  }

  *ptrVec[i] = 10;
  //ptrVec[i][0] = 10;

  printf("%d thingys", *ptrVec[4]);

  return 0;
}
