#include<stdio.h>

void func1(){
    int inputInt;
    scanf("Enter a number %d", &inputInt);

    if (inputInt < 0) {
      printf("Branch 0 [ < 0 ]");
    } else if (inputInt == 0) {
        func1();
    } else
      printf("Branch 2 [ > 0 ]");

    return;
}

void func2(int function2Arg) {
    if(function2Arg > 0){
        printf("function2Arg > 0");
    }
    else {
        // Nothing happens here
    }
}


int func3(int *function3Arg) {
    return ++*function3Arg;
    
}

int main(){

    int arg = 5;
    func1();
    func2(arg);
    arg = arg + 32;
    arg = func3(&arg);
    printf("%d",arg);

    return 0;

}
