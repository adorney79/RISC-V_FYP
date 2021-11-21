#include <stdio.h>
#include "encoding.h"
#define NUM_TESTS 100

float count(float val){
	 //unsigned long start, end;
        
	float rd;
	float rs2=0.0;
	const float three_halve=1.5;
	float x2= val*0.5;
	//start=rdcycle();
	asm volatile(".insn r CUSTOM_0,0x2,0,%0,%1,%2":"=r"(rd):"r"(val),"r"(rs2));
	//end=rdcycle();
	//printf("instruction execution took %lu cycles\n",end-start);
	rd=rd*(three_halve-(x2*rd*rd));
	return rd;
}
int main(){
	unsigned long start, end;
	
	float  x;
	for(float i=1.0;i<NUM_TESTS;i++){
	start=rdcycle();
		x=count(i);
	//	printf("sqrt of %lf returns %lf \n",i,x);
	
	end=rdcycle();
	printf("scie execution took %lu cycles\n",end-start);
	}
	return 0;
}


