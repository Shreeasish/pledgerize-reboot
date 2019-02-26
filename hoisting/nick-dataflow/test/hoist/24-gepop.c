#include<stdio.h>

struct {
  int x;
  int y;
  int z;
} global;

char* hello_message = "hello";

void
bar(int* x) {
  global.x = *x;
}

int*
get() {
  return &global.x;
}

void
myPrinter(char* s) {
  printf(s, "%s");
  return;
}


int main() { 
  int j;
  int* p = &j;
  scanf("%d", p);
  bar(p);
  
  if (*(get()) > 10) {
    myPrinter("well then");
  }
  return 0;
}
