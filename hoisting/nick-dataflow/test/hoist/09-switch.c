#include<stdio.h>

int main() {
  int a;
  scanf("%d", &a);
  int b;
  switch(a + 1) {
    case 1:
      b = a + 1;
      break;

    case -1.4:
      b = a*2;
      break;

    case 3:
      b = a - 1;
      break;

    default:
      b = 0;
  }

  printf("%d", b);

  return 0;
}
