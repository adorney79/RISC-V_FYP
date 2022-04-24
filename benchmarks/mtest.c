#include "rocc.h"
#include <stdio.h>
#include <stdlib.h>
#include "encoding.h"
#include <time.h>
#define MSIZE 8
#define PRINTSTATS 1
#define BMARK 1
#define Transpose 1

//test for matrix accelerators.
/*
Note: The static and inline keywords are used in some of the provided RoCC tests
and were used for this benchmark and the regex benchmark because it slightly improved performance.
Its probably due to the inlining but removing the static keyword sometimes led to strange behavior
*/

//used to check when accelerator is finished
static inline unsigned long read(int idx)
{
	unsigned long value;
	ROCC_INSTRUCTION_DSS(0, value, 0, idx, 5);
	return value;
}
//these functions specify the dimesions of each matrix and their addresses
static inline void set_x_dim(int l, int w)
{

        asm volatile ("fence");
        ROCC_INSTRUCTION_SS(0,l, w, 0);
}

static inline void set_y_dim(int l, int w)
{

        asm volatile ("fence");
        ROCC_INSTRUCTION_SS(0,l, w, 1);
}

static inline void set_addr(void *x_addr, void *y_addr)
{

        asm volatile ("fence");
        ROCC_INSTRUCTION_SS(0,(uintptr_t) x_addr,(uintptr_t) y_addr, 2);
}
static inline void set_out(void *ptr,void *copy)
{

        asm volatile ("fence");
        ROCC_INSTRUCTION_SS(0,(uintptr_t) ptr, (uintptr_t) copy, 3);
}



void accel();
void bmark();
void printm(unsigned long input[MSIZE][MSIZE]);
//using global variables here for the sake of ease
unsigned long X[MSIZE][MSIZE];
unsigned long Y[MSIZE][MSIZE];
unsigned long Z1[MSIZE][MSIZE];
unsigned long COPY[MSIZE][MSIZE];
unsigned long COPY2[MSIZE][MSIZE];
unsigned long Z2[MSIZE][MSIZE];

void accel(){
	 unsigned long result;
        unsigned long start,end;
        start=rdcycle();
        set_x_dim(MSIZE,MSIZE);
        set_y_dim(MSIZE,MSIZE);
        set_addr(&X[0][0],&Y[0][0]);
        set_out(&Z1[0][0],&COPY[0][0]);
        /*this result value isn't needed but it ensures the compiler doesn't
          optimise and call the rdcycle function before the accelerator actually finishes
        */
        result = read(0);
        end=rdcycle();
        printf("results is %lu \n",result);
#if PRINTSTATS
       printm(Z1);
#endif
        printf("rocc execution took %lu cycles \n",end-start);

}
//simply performs regular matrix multiplication
void bmark(){
       unsigned long result;
        unsigned long start,end;
        start=rdcycle();
        for(int i=0;i<MSIZE;i++){
		for(int j=0;j<MSIZE;j++){
			for(int k=0;k<MSIZE;k++){
				Z2[i][j]+=X[i][k]*Y[k][j];
			}
		}
	}
	end=rdcycle();
#if PRINTSTATS
       	printm(Z2);
#endif	
        printf("bmark execution took %lu cycles \n",end-start);

}
//same as bmark but transposes matrix first
void transpose(){
	unsigned long result;
        unsigned long start,end;
        start=rdcycle();

	for(int i=0;i<MSIZE;i++){
                for(int j=0;j<MSIZE;j++){
			COPY2[j][i]=Y[i][j];
                }
        }
        for(int i=0;i<MSIZE;i++){
                for(int j=0;j<MSIZE;j++){
                        for(int k=0;k<MSIZE;k++){
		
                                Z2[i][j]+=X[i][k]*COPY2[j][k];
                        }
                }
        }
        end=rdcycle();
#if PRINTSTATS
	printf("copy:\n");
	printm(COPY2);
	printf("z2:\n");
        printm(Z2);
#endif
        printf("transpose execution took %lu cycles \n",end-start);


}
//prints the matrix
void printm(unsigned long input[MSIZE][MSIZE]){
	for(int i=0;i<MSIZE;i++){
                for(int j=0;j<MSIZE;j++){
                        printf("%lu\t",input[i][j]);
                        }
                printf("\n");
                }


}
int main()
{       
        //generates matrices based on MSIZE
        // seeding doesn't seem to actually do anything since the RTL sets up a fresh
        // environment each time.
	srand(time(0));
	for(int i=0;i<MSIZE;i++){
                for(int j=0;j<MSIZE;j++){
			X[i][j]=rand()%1000;
			Y[i][j]=rand()%1000;
                        }
                }
#if PRINTSTATS
        printm(X);
	printf("multiplied by \n");
        printm(Y);
	printf("gives:\n");
        #endif

	accel();
#if BMARK
	bmark();
#endif
#if TRANSPOSE
	transpose();
#endif
	return 0;
}
