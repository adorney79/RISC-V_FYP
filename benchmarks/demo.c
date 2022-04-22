#include "rocc.h"
#include <stdio.h>
#include <stdlib.h>
#include "encoding.h"

//basic test for demo class
static inline unsigned long read()
{
        unsigned long value;
        ROCC_INSTRUCTION_DSS(0, value, 0, 0, 1);
        return value;
}
static inline void set_addr(void *addr)
{

        asm volatile("fence");
        ROCC_INSTRUCTION_SS(0, (uintptr_t)addr, (uintptr_t)addr, 2);
}
static inline void reset(){
	asm volatile("fence");
	ROCC_INSTRUCTION(0, 0)
}
int main(){
	unsigned long value=5;
	unsigned long result;
	unsigned long start,end;
        start=rdcycle();
	//set an address
	set_addr(&value);
	//read the result
	result=read();
	//reset the accelerator
	reset();
	end=rdcycle();
	printf("%lu plus 1 gives %lu\n",value,result);
	printf("rocc execution took %lu cycles \n",end-start);
	return 0;
}
