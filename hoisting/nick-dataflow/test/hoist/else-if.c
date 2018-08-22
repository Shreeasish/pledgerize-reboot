#include<stdio.h>

void func1(){

    return;
}

int main(){

    int inputInt;
    scanf("Enter a number %d", &inputInt);

    if (inputInt < 0) {
      printf("Branch 0 [ < 0 ]");
    } else if (inputInt == 0) {
        func1();
    } else
      printf("Branch 2 [ > 0 ]");

    return 0;
}
