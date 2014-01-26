package sLaCa3

object count01sTest extends sLaCa3{
	def main(args: Array[String]): Unit = {
			mem(0x3100) = 0x424F;						//0100001001001111	Expect 4
			//mem(0x3100) = -1;							//1111111111111111	Expect 0
			//mem(0x3100) = 1;							//0000000000000001	Expect 1
			//mem(0x3100) = java.lang.Short.MIN_VALUE;	//1000000000000000	Expect 0
			//mem(0x3100) = 0;							//0000000000000000	Expect 0
			
			dot ORIG 0x3000;
			LDI	R0 "IADDR"; //R0 gets the input value and is shifted left
			LD R4 "MASK";
			ADD	R3 R3 I8;
			ADD	R3 R3 I8; //R3 will count down to zero to stop looping
	"AGAIN"	vvv;
			ADD	R3 R3 I0;
			BR z "END";
			AND	R1 R0 R4; //R1 gets the value of the mask on top of the input
			BR z "FOUND0";
			ADD	R0 R0 R0;
			ADD	R3 R3 I_1;
			BR nzp "AGAIN"	

	"FOUND0"vvv;
			ADD	R0 R0 R0;
			ADD	R3 R3 I_1;
			AND	R1 R0 R4;
			BR np "YES";
			BR nzp "AGAIN";

	"YES"	vvv;	
			ADD	R2 R2 I1;
			ADD	R0 R0 R0;
			ADD	R3 R3 I_1;
			BR nzp "AGAIN";

	"END"	vvv;
			STI	R2 "OADDR";
			TRAP	x25;

	"MASK"	FILL	0x8000;
	"IADDR"	FILL	0x3100;
	"OADDR"	FILL	0x3101;
			dot END;
			println("Memory at x3100: " + mem(0x3100))
			println("Memory at x3101: " + mem(0x3101))
	}
}