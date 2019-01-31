#include<stdio.h>

int main() {
  int x;
  scanf("%d", &x);

  for (int i = x; i < 10;  i++) {
    x = i*x;
  }

  if(x%2) {
    printf("\nFrom if block");
  }
  else {
    printf("\nFrom else block");
  }

  return 0;
}

