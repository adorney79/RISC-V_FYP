#include <stdio.h>
#include "encoding.h"
#define NUM_TESTS 100

unsigned int count(unsigned int val){
	unsigned int rd=0;
	while(val){
		rd+=val&1;
		val>>=1;
	}
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
	printf("benchmark execution took %lu cycles\n",end-start);
	return 0;
}

