#include <stdio.h>
#include "encoding.h"
#define BMARK 1
#define ROCC 1
#define SCIE 0
#define PRINT_STATS 0
#define NUM_TESTS 100
//benchmark for popcount instruction

//used for RoCC accelerator
unsigned int rocc_count(unsigned int val){
	unsigned int rd;
	int rs2=0;
	asm volatile(".insn r CUSTOM_0,0x7,0x0,%0,%1,%2":"=r"(rd):"r"(val),"r"(rs2));
	return rd;
}

//counts bits using base ISA
unsigned int bmark_count(unsigned int val){
	unsigned int rd=0;
	while(val){
		rd+=val&1;
		val>>=1;
	}
	return rd;
}

//uses SCIE popcount instruction
unsigned int scie_count(unsigned int val){
	unsigned int rd;
	int rs2=0;
	asm volatile(".insn r CUSTOM_0,0x2,0,%0,%1,%2":"=r"(rd):"r"(val),"r"(rs2));
	return rd;
}

// baseline benchmark for popcount
void bmark(){
	unsigned long start, end;
	unsigned int x;
	start=rdcycle();
#if PRINT_STATS
	printf("output for bmark \n");
#endif
	for(int i=0;i<NUM_TESTS;i++){
		x=bmark_count(i);


#if PRINT_STATS
		printf("result is %d \n",x);
#endif
	}
	end=rdcycle();
	printf("benchmark execution took %lu cycles\n",end-start);
}

//calls accelerator for each number from 0-n
void rocc_accel(){
	unsigned long start, end;
	unsigned int x;
	start=rdcycle();
#if PRINT_STATS
	printf("output for rocc \n");
#endif
	for(int i=0;i<NUM_TESTS;i++){
		x=rocc_count(i);
#if PRINT_STATS
		printf("result is %d \n",x);
#endif
	}
	end=rdcycle();
	printf("RoCC execution took %lu cycles\n",end-start);
}

//same as rocc_accel but uses SCIE
void scie_accel(){
	unsigned long start, end;
	unsigned int x;
	start=rdcycle();
#if PRINT_STATS
	printf("output for scie \n");
#endif
	for(int i=0;i<NUM_TESTS;i++){
		x=scie_count(i);
#if PRINT_STATS
		printf("result is %d \n",x);
#endif
	}
	end=rdcycle();
	printf("scie execution took %lu cycles\n",end-start);
}

int main(){
#if ROCC
	rocc_accel();
#endif
#if SCIE
	scie_accel();
#endif
#if BMARK
	bmark();
#endif
	return 0;
}
