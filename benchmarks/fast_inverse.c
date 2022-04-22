#include <stdio.h>
#include "encoding.h"
#define NUM_TESTS 100 
#define PRINTSTATS 0
void bmark();

// code for fast inverse square root (not my own)
float  count( float val){
	long i;
	float x2, y;
	const float threehalfs = 1.5F;
	x2 = val * 0.5F;
	y  = val;
	i  = * ( long * ) &y;                       
	i  = 0x5f3759df - ( i >> 1 );              
	y  = * ( float * ) &i;
	y  = y * ( threehalfs - ( x2 * y * y ) );   
	return y;
}
int main(){
        bmark();
        return 0;
}

void bmark(){

	volatile unsigned long start, end;
        float  x;
        start=rdcycle();
        for(int i=1;i<=NUM_TESTS;i++){
                x=count((float)i);
#if PRINTSTATS
		printf("%lu \n",i);
#endif
        }
	asm("fence");
        end=rdcycle();
        printf("fast inverse execution took %lu cycles\n",end-start);

}

