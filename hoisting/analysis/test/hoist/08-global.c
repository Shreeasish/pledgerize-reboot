#include<stdio.h>
int themostimportantvariable;

int main(int argc, char** argv) {
  int index = 6;
  if (argv[6][0] > 6) {
    themostimportantvariable = index;
  }
  else {
    themostimportantvariable = index*2;
  }

  if (themostimportantvariable > 4) {
    printf("themostimportantvariable");
  }

  return 0;
}
