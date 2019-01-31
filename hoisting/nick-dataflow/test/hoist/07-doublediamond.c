#include<stdio.h>

int
special_call(int* x, int* y) {
  *x = *x + *y;
  return 0;
}

int main(){
  int x;
  scanf("%d", &x);
  int y = 5;

  if (x > y) {
    y = x+1;
  }
  else {
    x = y+1;
  }

  x++; y++;

  if (x > y + 5) {
    printf("Bottom diamond, True Branch");
    //special_call(&x,&y);
  }
  else {
    y=4;
  }
}
