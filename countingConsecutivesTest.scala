package sLaCa3

/**
 * This sLaCa3 program counts consecutive ones in the binary string stored in x3100.
 * It stores the highest count in memory location x3101.
 */

object countingConsecutivesTest extends sLaCa3{
	def main(args: Array[String]): Unit = {
			//Comment out all but one of these lines to select test input
			mem(0x3100) = 0x424F;				//0100001001001111	Expect 4
			//mem(0x3100) = -1;				//1111111111111111	Expect 0
			//mem(0x3100) = 1;				//0000000000000001	Expect 15
			//mem(0x3100) = java.lang.Short.MIN_VALUE;	//1000000000000000	Expect 15
			//mem(0x3100) = 0;				//1000000000000000	Expect 16
			 
			 
			dot ORIG 0x3000;
		     
			LDI	R0 "INPUT";
			ADD	R2 R2 I8;
			ADD	R2 R2 I8;
			LD	R5 "MASK";
	"AGAIN"	 vvv; 
			ADD	R2 R2 I0;
			BR z	"OVER";
			AND	R1 R0 R5;
			BR z	"FOUND0";
			ADD	R0 R0 R0;
			AND	R4 R4 I0;
			ADD	R2 R2 I_1;
			BR nzp	"AGAIN";
	"FOUND0" vvv;
			ADD	R0 R0 R0;
			ADD	R4 R4 I1;
			NOT	R7 R3;
			ADD	R7 R7 I1;
			ADD	R6 R4 R7;
			BR p	"NEW";
			ADD	R2 R2 I_1;
			BR nzp	"AGAIN";
	"NEW"	 vvv;	
			AND	R3 R3 I0;
			ADD	R3 R4 R3;
			ADD	R2 R2 I_1;
			BR nzp	"AGAIN";
	
	"OVER"	 vvv;
			STI	R3 "OUTPUT";
			TRAP	x25;


	"INPUT"	 FILL 0x3100;
	"MASK"	 FILL 0x8000;
	"OUTPUT" FILL 0x3101;
			dot END;
			println("Memory at x3101: " + mem(0x3101))
			//regs.foreach(println)
			//printLabels
			//mem.foreach(println)
	}
}
