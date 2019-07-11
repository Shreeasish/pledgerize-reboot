#include<stdio.h>

int global = 20;

int
foo(int* x) {
  *x = *x + global;
  return *x;
}


int main() {
  int arr[] = {0, 1, 2, 3, 4, 5};
  foo(&arr[3]);

  if (arr[3] < 200) {
    int i = 0;
    for (i = 0; i < 5; i++) {
      printf("%d\t", arr[i]);
    }
  }

  return 0;
}
