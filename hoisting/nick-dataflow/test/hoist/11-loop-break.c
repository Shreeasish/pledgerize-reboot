#include<stdio.h>

int main( ) {
  int x;
  scanf("%d",&x);
  int y = 7;

  int i = x;
  while ( i > 0 ) {
    if (i == 5) {
      break;
    }
    i--;
    x++;

  }

  if (x > y) {
    printf("Hello World %d", x);
  }

  return 0;
}
