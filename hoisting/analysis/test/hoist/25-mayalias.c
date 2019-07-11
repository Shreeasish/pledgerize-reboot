
#include<stdio.h>
struct mystruct {
  int x;
  int y;
} global;

void
obfuscate() {
  struct mystruct* ptrToGlobal = &global;
  if (ptrToGlobal->y > 2) {
    printf("Something");
  } else {
    ptrToGlobal->x++;
  }
}


int main(int argc, char** argv) {
  int index = 6;
  global.x = index;
  global.y = index + 1;
  struct mystruct* ptrToGlobal = &global;
  ptrToGlobal->x = 6;
  obfuscate();
  return 0;
}
