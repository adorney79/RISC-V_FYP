#include <stdio.h>
#include "encoding.h"
#include <math.h>
#define NUM_TESTS 100

float count(float val){
        float rd;
        rd=1/sqrt(val);
        return rd;
}
int main(){
        unsigned long start, end;
        start=rdcycle();
        float  x;
        for(float i=1.0;i<NUM_TESTS;i++){
                x=count(i);
               // printf("sqrt of %lf gives %lf \n",i,x);
        }
        end=rdcycle();
        printf("scie execution took %lu cycles\n",end-start);
        return 0;
}


