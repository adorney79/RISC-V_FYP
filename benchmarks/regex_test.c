#include "rocc.h"
//#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "encoding.h"
#include <sys/types.h>
void accel();
void bmark();
unsigned int compute();
//gets result from accelerator
static inline unsigned long read()
{
	int value;
	ROCC_INSTRUCTION_DSS(0, value, 0, 0, 2);
	return value;
}
//sets addresses and begins execution
static inline void set_addr(void *start_addr, void *stop_addr)
{

	asm volatile("fence");
	ROCC_INSTRUCTION_SS(0, (uintptr_t)start_addr, (uintptr_t)stop_addr, 1);
}

// the string to parse
unsigned char mini_stonks[] = "aria-label=\"S&amp;P 500\"><h3 class=\"Maw(160px)\"><a href=\"/quote/\%5EGSPC?p=\%5EGSPC\" title=\"S&amp;P 500\" class=\"Fz(s) Ell Fw(600) C($linkColor)\" ariaLabel=\"S&amp;P 500 has decreased by -1.30\% or -55.21 points to 4,204.31 points\">S&amp;P 500</a><br/><fin-streamer class=\"Fz(s) Mt(4px) Mb(0px) Fw(b) D(ib)\" data-symbol=\"^GSPC\" data-field=\"regularMarketPrice\" data-trend=\"none\" value=\"4204.31\" active=\"true\">4,204.31";

int main()
{
	bmark();
	accel();
	return 0;
}

void bmark()
{
	unsigned long start, end;
	start = rdcycle();
	unsigned int guard=compute();
	end = rdcycle();
	printf("result is: %u ", guard);
	printf("bmark execution took %lu cycles \n", end - start);
}
/*
Forgive the extremely ugly code but regex.h wasn't supported for bare metal
*/
unsigned int compute(){
	unsigned char matches[] = "\"S&amp;P 500\"regularMarketPricevalue=\"";
	int pointer = 0;
	int lst_idx = 0;
	unsigned int guard = 0;
	int length = sizeof(mini_stonks) / sizeof(char);

	while (!guard)
	{
		//when EOF reached exit
		if (pointer == length)
		{
			break;
		}
		//potential match
		else if (matches[0] == mini_stonks[pointer])
		{
			lst_idx++;
			pointer++;
			for (int i = pointer; i < length; i++)
			{	//if first set of values are matched continue to .*?
				if (lst_idx == 13)
				{
					if (matches[13] == mini_stonks[i])
					{
						lst_idx++;
						i++;
						//continues parsing until EOF or potential match
						for (int j = i; j < length; j++)
						{
							//if a full match is found proceed to next section
							if (lst_idx == 31)
							{
			
								if (matches[31] == mini_stonks[j])
								{
									lst_idx++;
									j++;
									//continues parsing until EOF or match
									for (int z = j; z < length; z++)
									{
										//match is found so exit
										if (lst_idx == 37)
										{
											guard = 1;
											return guard;
										}
										else if (matches[lst_idx] == mini_stonks[z])
										{
											lst_idx++;
										}
										else
										{
											lst_idx = 31;
										}
									}
								}
							}
							// keeps incrementing as long as match satisfied
							else if (matches[lst_idx] == mini_stonks[j])
							{
								lst_idx++;
							}
							//resets count if a potential match fails
							else
							{
								lst_idx = 13;
							}
						}
					}
				}
				//matches first set of values until 13
				else if (matches[lst_idx] == mini_stonks[i])
				{
					lst_idx++;
				}
				//potential match failure so backtrack
				else
				{
					lst_idx = 0;
					break;
				}
			}
		}
		//continue parsing for potential match
		else
		{
			pointer++;
		}
	}
	//EOF reached
	return guard;
}
//simply calls accelerator
void accel()
{
	unsigned long start, end;
	start = rdcycle();
	set_addr(&mini_stonks[0], &mini_stonks[411]);

	int result = read(0);
	end = rdcycle();
	printf("result is: %u ", result);
	printf("rocc execution took %lu cycles \n", end - start);
}
