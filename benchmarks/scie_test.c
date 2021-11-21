#include <stdio.h>
#include "encoding.h"
#define NUM_TESTS 100

unsigned int count(unsigned int val){
	unsigned int rd;
	int rs2=0;
	asm volatile(".insn r CUSTOM_0,0x2,0,%0,%1,%2":"=r"(rd):"r"(val),"r"(rs2));
	return rd;
}
int main(){
	unsigned long start, end;
	unsigned int x;
	start=rdcycle();
	for(int i=0;i<NUM_TESTS;i++){
		x=count(i);
		/*printf("result is %d \n",x);*/
	}
	end=rdcycle();
	printf("scie execution took %lu cycles\n",end-start);
	return 0;
}

