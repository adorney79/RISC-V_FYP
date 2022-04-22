#include <stdio.h>
#include "encoding.h"
#include <math.h>
#define NUM_TESTS 100
#define PRINTSTATS 0

//test for inverse root with ISA square root instruction 
float count(volatile float val){
        float rd;
        rd=1/sqrt(val);
        return rd;
}
int main(){
        unsigned long start, end;
        start=rdcycle();
        float  x;
        for(int i=0;i<=NUM_TESTS;i++){
                x=count((float)i);
#if PRINTSTATS
               printf("sqrt of %lf gives %lf \n",i,x);
#endif
        }
	asm("fence");
        end=rdcycle()-start;
        printf("bmark execution took %lu cycles\n",end);
        return 0;
}


