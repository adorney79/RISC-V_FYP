#include <stdio.h>
#include "encoding.h"
#define NUM_TESTS 100
#define PRINTSTATS 0
//SCIE fast inverse test
float count(volatile float val){
        
	float rd;
	float rs2=0.0;
	const float three_halve=1.5;
	float x2= val*0.5;
	asm (".insn r CUSTOM_0,0x2,0,%0,%1,%2":"=r"(rd):"r"(val),"r"(rs2));

	rd=rd*(three_halve-(x2*rd*rd));
	return rd;
}
int main(){
	unsigned long start, end;
	float  x;
	start=rdcycle();
	for(int i=1;i<=NUM_TESTS;i++){
		x=count((float)i);
#if PRINTSTATS

		printf("inv sqrt of %lf returns %lf \n",i,x);
#endif
	}
	asm("fence");
	end=rdcycle();
	printf("scie execution took %lu cycles\n",end-start);
	
	return 0;
}


