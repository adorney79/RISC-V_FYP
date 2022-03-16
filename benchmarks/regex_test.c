#include "rocc.h"
//#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "encoding.h"
//#include <sys/types.h>
void accel();
void bmark();
static inline unsigned long read()
{
	int value;
	ROCC_INSTRUCTION_DSS(0, value, 0, 0, 2);
	return value;
}
static inline void set_addr(void *start_addr,void *stop_addr )
{

        asm volatile ("fence");
        ROCC_INSTRUCTION_SS(0,(uintptr_t) start_addr,(uintptr_t) stop_addr, 1);
}

unsigned char stonks[]="d=\"M24 20c-2.205 0-4 1.795-4 4s1.795 4 4 4 4-1.795 4-4-1.795-4-4-4M37.12 24.032c0-4.09-1.764-7.896-4.78-10.537-.83-.727-2.094-.644-2.822.187-.727.832-.644 2.095.187 2.823 2.158 1.89 3.416 4.604 3.416 7.527 0 2.932-1.265 5.654-3.434 7.543-.833.726-.92 1.99-.194 2.822.725.833 1.99.92 2.822.194 3.032-2.64 4.807-6.458 4.807-10.558zM45.097 23.982c0-6.996-3.29-13.45-8.77-17.58-.883-.664-2.137-.488-2.802.394-.664.882-.488 2.136.394 2.8 4.487 3.384 7.177 8.66 7.177 14.386 0 5.775-2.736 11.09-7.288 14.468-.89.658-1.074 1.91-.416 2.798.658.887 1.91 1.073 2.797.415 5.56-4.124 8.907-10.625 8.907-17.68zM15 24.032c0-2.923 1.26-5.638 3.416-7.527.83-.728.915-1.99.187-2.823-.727-.83-1.99-.914-2.822-.187-3.015 2.64-4.78 6.448-4.78 10.537 0 4.1 1.776 7.918 4.808 10.56.833.725 2.096.638 2.822-.195.725-.833.638-2.096-.195-2.822-2.17-1.89-3.435-4.61-3.435-7.543zM7 23.982c0-5.726 2.69-11.002 7.178-14.385.882-.665 1.06-1.92.394-2.8-.665-.883-1.92-1.06-2.8-.394C6.29 10.533 3 16.986 3 23.983c0 7.055 3.347 13.556 8.906 17.68.887.658 2.14.472 2.798-.415.658-.887.472-2.14-.415-2.798C9.735 35.073 7 29.757 7 23.982z\"></path></svg><span class=\"C($tertiaryColor)\" data-id=\"mk-msg\">U.S. markets closed</span></div></div></div><div class=\"Whs(nw) D(ib) Bgc($lv2BgColor) W(100%) Bxz(bb)\" id=\"market-summary\" aria-label=\"Market summary containing a list of 15 items\" tabindex=\"0\"><div class=\"Pos(r) Bxz(bb) Mstart(a) Mend(a) Ov(h)\"><div class=\"D(ib) Fl(start)  W(100%)\" data-yaft-module=\"tdv2-applet-MarketSummary\"><div class=\"Carousel-Mask Pos(r) Ov(h) market-summary M(0) Pos(r) Ov(h) D(ib) Va(t)\" style=\"width:1176px\"><ul class=\"Carousel-Slider Pos(r) Whs(nw)\" style=\"margin-left:0;margin-right:-2px\"><li style=\"width:15.3%\" class=\"D(ib) Bxz(bb) Bdc($seperatorColor) Mend(16px) BdEnd\" id=\"marketsummary-itm-0\" aria-label=\"S&amp;P 500\"><h3 class=\"Maw(160px)\"><a href=\"/quote/\%5EGSPC?p=\%5EGSPC\" title=\"S&amp;P 500\" class=\"Fz(s) Ell Fw(600) C($linkColor)\" ariaLabel=\"S&amp;P 500 has decreased by -1.30\% or -55.21 points to 4,204.31 points\">S&amp;P 500</a><br/><fin-streamer class=\"Fz(s) Mt(4px) Mb(0px) Fw(b) D(ib)\" data-symbol=\"^GSPC\" data-field=\"regularMarketPrice\" data-trend=\"none\" value=\"4204.31\" active=\"true\">4,204.31</fin-streamer><div class=\"Fz(xs) Fw(b)\"><fin-streamer class=\"Mend(2px)\" data-symbol=\"^GSPC\" data-field=\"regularMarketChange\" data-trend=\"txt\" value=\"-55.20996\" active=\"true\"><span class=\"C($negativeColor)\">-55.21</span></fin-streamer><fin-streamer data-symbol=\"^GSPC\" data-field=\"regularMarketChangePercent\" data-trend=\"txt\" data-template=\"({fmt})\" value=\"-1.2961545\" active=\"true\"><span class=\"C";	

int main(){
	bmark();
 	accel();
       	return 0;

}
unsigned  char test[]="azxycabc12";

void bmark(){
	unsigned long result=0;
        unsigned long start,end;
        start=rdcycle();
        for(int i=0;i<10;i++){
		if(test[i]=='a'){
			if(test[i+1]=='b'){
				if(test[i+2]){
					result=1;
				}
			}
		}
	}
        end=rdcycle();
	printf("result is: %u",result);
        printf("bmark execution took %lu cycles \n",end-start);


}
void accel(){
	unsigned long start,end;
	start=rdcycle();
	set_addr(&test[0],&test[9]);
	
	int result=read(0);
	end=rdcycle();
	printf("result is: %u",result);
	printf("rocc execution took %lu cycles \n",end-start);

}


