#include <stdio.h>
#include "encoding.h"
#include <math.h>
#define NUM_TESTS 100

float count(float val){
//	unsigned long start, end;
	long i;
	float x2, y;
	const float threehalfs = 1.5F;
	//start=rdcycle();
	x2 = val * 0.5F;
	y  = val;
	i  = * ( long * ) &y;                       // evil floating point bit level hacking
	i  = 0x5f3759df - ( i >> 1 );               // what the fuck?
	y  = * ( float * ) &i;
//	end=rdcycle();
//	printf("all before newton took %lu cycles\n",end-start);
	y  = y * ( threehalfs - ( x2 * y * y ) );   // 1st iteration
//	y  = y * ( threehalfs - ( x2 * y * y ) );   // 2nd iteration, this can be removed

	return y;
}
int main(){
        unsigned long start, end;
        float  x;
        for(float i=1.0;i<NUM_TESTS;i++){
	start=rdcycle();
                x=count(i);
               // printf("sqrt of %lf gives %lf \n",i,x);
        
        end=rdcycle();
        printf("fast inverse execution took %lu cycles\n",end-start);
	}
        return 0;
}



