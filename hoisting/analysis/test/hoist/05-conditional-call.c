#include<stdio.h>

int main()
{
    int inputInt;
    int dependentInt;
    scanf("%d",&inputInt);

    if(inputInt > 5){
        dependentInt = 2;
    }
    else{
        dependentInt = 1;
    }

    inputInt = inputInt +1;

    if(dependentInt == 2){
        printf("Dependent Int = 2");
    }
    else
    {
        //nothing here wolololo
        inputInt = dependentInt + inputInt;
        printf("woot woot");
    }


    return 0;
}
