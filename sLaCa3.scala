package sLaCa3

import scala.collection.mutable.HashMap

/**
 * sLaCa3 is an internal DSL implementation for the assembly language of LC3, an educational tool
 * created in part by Yale Patt of the University of Texas at Austin and used in the
 * Electrical Engineering curriculum. More detail of LC3's specifications can be found
 * in Introduction to Computing Systems by Patt and Patel.
 * 
 * Programs written by sLaCa3 have no way of interacting with the system or a console.
 * To observe the execution of the program, the variables mem, regs, and flag must be examined.
 * 
 * sLaCa3 uses Scala's acceptance of method calls without using parentheses or periods
 * to make a better visual match of genuine LC3 code. Discrepancies with true LC3 syntax include:
 *    -Arguments delimited with space instead of comma
 *    -Lines need to be terminated with a semicolon
 *    -Immediate values indicated by character "I" instead of "#", and negative symbol is "_".
 *    -The assembler directives take different forms
 * 
 * The current implemented instructions include:
 *    -ADD <Destination register> <Source register> <Source register>
 *    -ADD <Destination register> <Source register> <Immediate from -16 to 15>
 *       ADD does addition between the last two arguments and stores the 16 least significant bits
 *    
 *    -AND <Destination register> <Source register> <Source register>
 *    -AND <Destination register> <Source register> <Immediate from -16 to 15>
 *       AND does bitwise and between the last two arguments and stores into destination
 *     
 *    -BR <Branch Condition> "<Label>"
 *       BR will branch to the location indicated by the label depending on the flag.
 *       Branch conditions having "n" will branch with negative value of flag,
 *       "p" branches for positive values, and "z" branches if flag == 0.
 *       
 *    -LD <Destination register> "<Label>"
 *       LD puts the value designated by the label into the destination
 *       
 *    -LDI <Destination register> "<Label>"
 *       LDI puts the value of the memory location designated by the label into the destination
 *       
 *    -NOT <Destination register> <Source register>
 *       NOT puts the inversion of the source's bits into the destination
 *       
 *    -ST
 *       ST puts the value of the source into the label
 *       
 *    -STI
 *       ST puts the value of the source into the memory designated by the label
 *       
 *    -TRAP
 *       Imitation of the system calls in the real LC3 language. Currently only supports
 *       trapvector value x25, for halting the program.
 *    
 */


class sLaCa3 {
	abstract sealed class Line
	case class Orig(dest: Int)
	case class AddReg(dest: Int, src1: Int, src2: Int) extends Line
	case class AddImm(dest: Int, src1: Int, src2: Int) extends Line
	case class AndReg(dest: Int, src1: Int, src2: Int) extends Line
	case class AndImm(dest: Int, src1: Int, src2: Int) extends Line
	case class Brn(label: String) extends Line
	case class Brz(label: String) extends Line
	case class Brp(label: String) extends Line
	case class Br(label: String) extends Line
	case class Brzp(label: String) extends Line
	case class Brnp(label: String) extends Line
	case class Brnz(label: String) extends Line
	case class Ld(dest: Int, label: String) extends Line
	case class Ldi(dest: Int, label: String) extends Line
	case class Ldr(dest: Int, base: Int, offset: Int) extends Line
	case class Not(dest: Int, src: Int) extends Line
	case class St(src: Int, label: String) extends Line
	case class Sti(src: Int, label: String) extends Line
	case class Str(src: Int, base: Int, offset: Int) extends Line
	case class Trap(vect: Int) extends Line
	
	var current = 0;						//describes which line the construction of the program is on.
	var memLoc = 0;							//holds address in memory the program will go into.
	var prog = new HashMap[Int, Line]		//contains program during runtime so branching can be evaluated
	var labels = new HashMap[String, Short] //contains the values each label corresponds to.
	var mem = new Array[Short](0x10000)		//contains the state of memory
	var regs = new Array[Short](8)			//contains the state of simulated registers
	var flag = 0							//contains the value of the branching flag
	
	//Debugging and examination method to look at the label assignments
	def printLabels() = { labels.foreach(println) }
	
	//Debugging and examination method to look at the registers and the flag
	def printRegs() = {
		for(i <- 0 to 7) { printf("R%d: 0x%x   ", i, regs(i)); }
		println("RFLAG: " + flag);
	}
	
	/**
	 * These assignments cause Scala to interpret registers in the form R<number> as integers,
	 * which is crucial to workings of the internal DSL
	 */
	val R0 = 0; val R1 = 1; val R2 = 2; val R3 = 3; val R4 = 4; val R5 = 5; val R6 = 6; val R7 = 7;
	
	/**
	 *The psuedo-ops (assembler directives) in LC3 use the form of "dot <op> <args>" in sLaCa3
	 */
	var dot = new dotOps;
	class dotOps {
	    //LC3 stores the program into memory in its machine code form. sLaCa3 does nothing useful here.
		def ORIG(addr: Int) = { memLoc = addr; }		
		//".END" marks the termination of an LC3 assembly file, so "dot END;" in sLaCa3 does the same.
		def END() = { executeAt(0,0); }
	}
	
	/**
	 * When the interpreter comes across a string in the beginning of the line, it will turn it into a labelHandler.
	 * The label can either refer to a line number within the assembly code or a magic number.
	 */
	implicit def string2labelHandler(s: String) = new labelHandler(s)
	class labelHandler (l: String) {
		var label = l
		def vvv() = { labels(l) = current.shortValue(); }
		def FILL(value: Int) = { labels(l) = value.shortValue(); }
	}
	
	/**
	 * This consumes the instruction "ADD" and its first and second arguments.
	 * The interpreter will interpret the first argument as a function,
	 * and the second argument as the parameter for said function.
	 * Afterwards, it constructs an addContainer to consume the third argument.
	 */
	var ADD = new addOps;
	class addOps() {
		def R0(i : Int) : addContainer = { return new addContainer(0,i); }
		def R1(i : Int) : addContainer = { return new addContainer(1,i); }
		def R2(i : Int) : addContainer = { return new addContainer(2,i); }
		def R3(i : Int) : addContainer = { return new addContainer(3,i); }
		def R4(i : Int) : addContainer = { return new addContainer(4,i); }
		def R5(i : Int) : addContainer = { return new addContainer(5,i); }
		def R6(i : Int) : addContainer = { return new addContainer(6,i); }
		def R7(i : Int) : addContainer = { return new addContainer(7,i); }
	}
	
	/**
	 * An instance of this class consumes the final argument of an ADD instruction.
	 * The interpreter will interpret the final argument as a function of addContainer.
	 */
	class addContainer(x: Int, y:Int) {
		var dest = x;
		var src1 = y;
		var src2 = 0;
		var isImmediate = 1;
		
		def putInstr() : Int = {
		    if (isImmediate == 1) {
		    	prog(current) = AddImm(dest, src1, src2)
		    } else {
		    	prog(current) = AddReg(dest, src1, src2)
		    }
			current += 1; return current;
		}
		
		//Value of the last argument is hard-coded into DSL as a function
		def R0() = { src2 = 0; isImmediate = 0;	putInstr();	}
		def R1() = { src2 = 1; isImmediate = 0;	putInstr(); }
		def R2() = { src2 = 2; isImmediate = 0; putInstr(); }
		def R3() = { src2 = 3; isImmediate = 0;	putInstr();	}
		def R4() = { src2 = 4; isImmediate = 0;	putInstr(); }
		def R1() = { src2 = 5; isImmediate = 0;	putInstr(); }
		def R2() = { src2 = 6; isImmediate = 0; putInstr(); }
		def R3() = { src2 = 7; isImmediate = 0;	putInstr();	}
		def R4() = { src2 = 8; isImmediate = 0;	putInstr(); }		
		//The following values correspond to the immediate values representable with 5 bits.
		def I0() 	=	{ src2 = 0; putInstr(); }
		def I1() 	=	{ src2 = 1; putInstr(); }
		def I2() 	=	{ src2 = 2; putInstr(); }
		def I3() 	=	{ src2 = 3; putInstr(); }
		def I4() 	=	{ src2 = 4; putInstr(); }
		def I5() 	= 	{ src2 = 5; putInstr(); }
		def I6() 	= 	{ src2 = 6; putInstr(); }
		def I7() 	=	{ src2 = 7; putInstr(); }
		def I8() 	= 	{ src2 = 8; putInstr(); }
		def I9() 	= 	{ src2 = 9; putInstr(); }
		def I10()	= 	{ src2 = 10; putInstr(); }
		def I11() 	= 	{ src2 = 11; putInstr(); }
		def I12() 	= 	{ src2 = 12; putInstr(); }
		def I13() 	= 	{ src2 = 13; putInstr(); }
		def I14() 	= 	{ src2 = 14; putInstr(); }
		def I15() 	= 	{ src2 = 15; putInstr(); }
		def I_16() 	=	{ src2 = -16; putInstr(); }
		def I_15() 	=	{ src2 = -15; putInstr(); }
		def I_14() 	=	{ src2 = -14; putInstr(); }
		def I_13() 	=	{ src2 = -13; putInstr(); }
		def I_12() 	=	{ src2 = -12; putInstr(); }
		def I_11() 	=	{ src2 = -11; putInstr(); }
		def I_10() 	=	{ src2 = -10; putInstr(); }
		def I_9() 	= 	{ src2 = -9; putInstr(); }
		def I_8() 	= 	{ src2 = -8; putInstr(); }
		def I_7() 	= 	{ src2 = -7; putInstr(); }
		def I_6() 	= 	{ src2 = -6; putInstr(); }
		def I_5() 	= 	{ src2 = -5; putInstr(); }
		def I_4() 	= 	{ src2 = -4; putInstr(); }
		def I_3() 	= 	{ src2 = -3; putInstr(); }
		def I_2() 	= 	{ src2 = -2; putInstr(); }
		def I_1() 	= 	{ src2 = -1; putInstr(); }
	}
	
	/**
	 * This consumes the instruction "AND" and its first and second arguments.
	 * The interpreter will interpret the first argument as a function,
	 * and the second argument as the parameter for said function.
	 * Afterwards, it constructs an andContainer to consume the third argument.
	 */
	var AND = new andOps;
	class andOps() {
		def R0(i : Int) : andContainer = { return new andContainer(0,i); }
		def R1(i : Int) : andContainer = { return new andContainer(1,i); }
		def R2(i : Int) : andContainer = { return new andContainer(2,i); }
		def R3(i : Int) : andContainer = { return new andContainer(3,i); }
		def R4(i : Int) : andContainer = { return new andContainer(4,i); }
		def R5(i : Int) : andContainer = { return new andContainer(5,i); }
		def R6(i : Int) : andContainer = { return new andContainer(6,i); }
		def R7(i : Int) : andContainer = { return new andContainer(7,i); }
	}
	
	/**
	 * An instance of this class consumes the final argument of an ADD instruction.
	 * The interpreter will interpret the final argument as a function of addContainer.
	 */
	class andContainer(x: Int, y:Int) {
		var dest = x
		var src1 = y
		var src2 = 0;
		var isImmediate = 1;
		
		def putInstr() : Int = {
		    if (isImmediate == 1) {
		    	prog(current) = AndImm(dest, src1, src2)
		    } else {
		    	prog(current) = AndReg(dest, src1, src2)
		    }
			current += 1
			return current
		}
		
		//Value of the last argument is hard-coded into DSL as a function
		def R0() = { src2 = 0; isImmediate = 0;	putInstr();	}
		def R1() = { src2 = 1; isImmediate = 0;	putInstr(); }
		def R2() = { src2 = 2; isImmediate = 0; putInstr(); }
		def R3() = { src2 = 3; isImmediate = 0;	putInstr();	}
		def R4() = { src2 = 4; isImmediate = 0;	putInstr(); }
		def R1() = { src2 = 5; isImmediate = 0;	putInstr(); }
		def R2() = { src2 = 6; isImmediate = 0; putInstr(); }
		def R3() = { src2 = 7; isImmediate = 0;	putInstr();	}
		def R4() = { src2 = 8; isImmediate = 0;	putInstr(); }		
		//The following values correspond to the immediate values representable with 5 bits.
		def I0() 	=	{ src2 = 0; putInstr(); }
		def I1() 	=	{ src2 = 1; putInstr(); }
		def I2() 	=	{ src2 = 2; putInstr(); }
		def I3() 	=	{ src2 = 3; putInstr(); }
		def I4() 	=	{ src2 = 4; putInstr(); }
		def I5() 	= 	{ src2 = 5; putInstr(); }
		def I6() 	= 	{ src2 = 6; putInstr(); }
		def I7() 	=	{ src2 = 7; putInstr(); }
		def I8() 	= 	{ src2 = 8; putInstr(); }
		def I9() 	= 	{ src2 = 9; putInstr(); }
		def I10()	= 	{ src2 = 10; putInstr(); }
		def I11() 	= 	{ src2 = 11; putInstr(); }
		def I12() 	= 	{ src2 = 12; putInstr(); }
		def I13() 	= 	{ src2 = 13; putInstr(); }
		def I14() 	= 	{ src2 = 14; putInstr(); }
		def I15() 	= 	{ src2 = 15; putInstr(); }
		def I_16() 	=	{ src2 = -16; putInstr(); }
		def I_15() 	=	{ src2 = -15; putInstr(); }
		def I_14() 	=	{ src2 = -14; putInstr(); }
		def I_13() 	=	{ src2 = -13; putInstr(); }
		def I_12() 	=	{ src2 = -12; putInstr(); }
		def I_11() 	=	{ src2 = -11; putInstr(); }
		def I_10() 	=	{ src2 = -10; putInstr(); }
		def I_9() 	= 	{ src2 = -9; putInstr(); }
		def I_8() 	= 	{ src2 = -8; putInstr(); }
		def I_7() 	= 	{ src2 = -7; putInstr(); }
		def I_6() 	= 	{ src2 = -6; putInstr(); }
		def I_5() 	= 	{ src2 = -5; putInstr(); }
		def I_4() 	= 	{ src2 = -4; putInstr(); }
		def I_3() 	= 	{ src2 = -3; putInstr(); }
		def I_2() 	= 	{ src2 = -2; putInstr(); }
		def I_1() 	= 	{ src2 = -1; putInstr(); }
	}
	
	/**
	 * This consumes the "BR" instruction as well as the branch condition and the label
	 */
	var BR = new brOps
	class brOps() {
		def n(label: String) = {
			prog(current) = Brn(label)
			current += 1
		}
		def z(label: String) = {
			prog(current) = Brz(label)
			current += 1
		}
		def p(label: String) = {
			prog(current) = Brp(label)
			current += 1
		}
		def nz(label: String) = {
			prog(current) = Brnz(label)
			current += 1
		}
		def zp(label: String) = {
			prog(current) = Brzp(label)
			current += 1
		}
		def np(label: String) = {
			prog(current) = Brnp(label)
			current += 1
		}
		def nzp(label: String) = {
			prog(current) = Br(label)
			current += 1
		}
	}
	
	/**
	 * This consumes the "LD" instruction as well as the destination register and the label
	 */
	var LD = new ldOps
	class ldOps {
		def R0(label: String) = { prog(current) = Ld(0, label); current += 1; }
		def R1(label: String) = { prog(current) = Ld(1, label);	current += 1; }
		def R2(label: String) = { prog(current) = Ld(2, label);	current += 1; }
		def R3(label: String) = { prog(current) = Ld(3, label);	current += 1; }
		def R4(label: String) = { prog(current) = Ld(4, label);	current += 1; }
		def R5(label: String) = { prog(current) = Ld(5, label);	current += 1; }
		def R6(label: String) = { prog(current) = Ld(6, label);	current += 1; }
		def R7(label: String) = { prog(current) = Ld(7, label);	current += 1; }
	}
	
	/**
	 * This consumes the "LDI" instruction as well as the destination register and the label
	 */
	var LDI = new ldiOps
	class ldiOps {
		def R0(label: String) = { prog(current) = Ldi(0, label); current += 1; }
		def R1(label: String) = { prog(current) = Ldi(1, label); current += 1; }
		def R2(label: String) = { prog(current) = Ldi(2, label); current += 1; }
		def R3(label: String) = { prog(current) = Ldi(3, label); current += 1; }
		def R4(label: String) = { prog(current) = Ldi(4, label); current += 1; }
		def R5(label: String) = { prog(current) = Ldi(5, label); current += 1; }
		def R6(label: String) = { prog(current) = Ldi(6, label); current += 1; }
		def R7(label: String) = { prog(current) = Ldi(7, label); current += 1; }
	}
	
	var LDR = new ldrOps
	class ldrOps {
		//LDR not implemented yet
	}
	
	/**
	 * This consumes the "NOT" instruction as well as the source and destination register
	 */
	var NOT = new notOps
	class notOps {
		def R0(src: Int) = { prog(current) = Not(0, src); current += 1; }
		def R1(src: Int) = { prog(current) = Not(1, src); current += 1; }
		def R2(src: Int) = { prog(current) = Not(2, src); current += 1; }
		def R3(src: Int) = { prog(current) = Not(3, src); current += 1; }
		def R4(src: Int) = { prog(current) = Not(4, src); current += 1; }
		def R5(src: Int) = { prog(current) = Not(5, src); current += 1; }
		def R6(src: Int) = { prog(current) = Not(6, src); current += 1; }
		def R7(src: Int) = { prog(current) = Not(7, src); current += 1; }
	}
	
	/**
	 * This consumes the "ST" instruction as well as the source register and the label
	 */
	var ST = new stOps
	class stOps {
		def R0(label: String) = { prog(current) = St(0, label);	current += 1; }
		def R1(label: String) = { prog(current) = St(1, label);	current += 1; }
		def R2(label: String) = { prog(current) = St(2, label);	current += 1; }
		def R3(label: String) = { prog(current) = St(3, label);	current += 1; }
		def R4(label: String) = { prog(current) = St(4, label);	current += 1; }
		def R5(label: String) = { prog(current) = St(5, label);	current += 1; }
		def R6(label: String) = { prog(current) = St(6, label);	current += 1; }
		def R7(label: String) = { prog(current) = St(7, label);	current += 1; }
	}
	/**
	 * This consumes the "STI" instruction as well as the source register and the label
	 */
	var STI = new stiOp
	class stiOp {
		def R0(label: String) = { prog(current) = Sti(0, label); current += 1; }
		def R1(label: String) = { prog(current) = Sti(1, label); current += 1; }
		def R2(label: String) = { prog(current) = Sti(2, label); current += 1; }
		def R3(label: String) = { prog(current) = Sti(3, label); current += 1; }
		def R4(label: String) = { prog(current) = Sti(4, label); current += 1; }
		def R5(label: String) = { prog(current) = Sti(5, label); current += 1; }
		def R6(label: String) = { prog(current) = Sti(6, label); current += 1; }
		def R7(label: String) = { prog(current) = Sti(7, label); current += 1; }
	}
	
	var STR = new strOp
	class strOp {
		//STR not implemented
	}
	
	/**
	 * This consumes the "TRAP" instruction as well as the trappvector
	 */
	var TRAP = new trapOp
	class trapOp {
		def x25() = { //x25 is the only trapvector currently allowed
			prog(current) = Trap(25);
			current += 1;
		}
	}
	
	/**
	 * The following code executes the program once it has been stored into the data structure.
	 */
	def executeAt(i: Int, steps: Int) {
		printRegs
	    println("About to execute at instruction " + i + ": " + prog(i))
		prog(i) match {
			case AddReg(dest: Int, src1: Int, src2: Int) => {
			    var temp = regs(src1) + regs(src2);
			    if (temp >= 0x10000) {
			    	temp % 0x10000
			    }
				regs(dest) = temp.shortValue
				flag = regs(dest)
				executeAt(i+1, steps+1)
			}
			case AddImm(dest: Int, src1: Int, src2: Int) => {
				var temp = regs(src1) + src2;
			    if (temp >= 0x10000) {
			    	temp % 0x10000
			    }
				regs(dest) = temp.shortValue
				flag = regs(dest)
				executeAt(i+1, steps+1)
			}
			case AndReg(dest: Int, src1: Int, src2: Int) => {
				regs(dest) = (regs(src1) & regs(src2)).shortValue
				flag = regs(dest)
				executeAt(i+1, steps+1)
			}
			case AndImm(dest: Int, src1: Int, src2: Int) => {
				regs(dest) = (regs(src1) & src2).shortValue
				flag = regs(dest)
				executeAt(i+1, steps+1)
			}
			case Brn(label: String) => {
				if (flag < 0) {
					executeAt(labels(label), steps+1)
				} else {
					executeAt(i+1, steps+1)
				}
			}
			case Brz(label: String) => {
				if (flag == 0) {
					executeAt(labels(label), steps+1)
				} else {
					executeAt(i+1, steps+1)
				}
			}
			case Brp(label: String) => {
				if (flag > 0) {
					executeAt(labels(label), steps+1)
				} else {
					executeAt(i+1, steps+1)
				}
			}
			case Br(label: String) => {
				executeAt(labels(label), steps+1)
			}
			case Brzp(label: String) => {
				if (flag >= 0) {
					executeAt(labels(label), steps+1)
				} else {
					executeAt(i+1, steps+1)
				}
			}
			case Brnp(label: String) => {
				if (flag != 0) {
					executeAt(labels(label), steps+1)
				} else {
					executeAt(i+1, steps+1)
				}
			}
			case Brnz(label: String) => {
				if (flag <= 0) {
					executeAt(labels(label), steps+1)
				} else {
					executeAt(i+1, steps+1)
				}
			}
			case Ld(dest: Int, label: String) => {
				regs(dest) = labels(label)
				flag = regs(dest)
				executeAt(i+1, steps+1)
			}
			case Ldi(dest: Int, label: String) => {
				regs(dest) = mem(labels(label))
				flag = regs(dest)
				executeAt(i+1, steps+1)
			}
			case Ldr(dest: Int, base: Int, offset: Int) => {
				regs(dest) = mem(regs(base) + offset)
				flag = regs(dest)
				executeAt(i+1, steps+1)
			}
			case Not (dest: Int, src: Int) => {
				regs(dest) = (~regs(src)).shortValue;
				flag = regs(dest)
				executeAt(i+1, steps+1)
			}
			case St (src: Int, label: String) => {
				labels(label) = regs(src);
				executeAt(i+1, steps+1)
			}
			case Sti (src: Int, label: String) => {
				mem(labels(label)) = regs(src)
				executeAt(i+1, steps+1)
			}
			case Str (src: Int, base: Int, offset: Int) => {
				mem(regs(base) + offset) = regs(src)
				executeAt(i+1, steps+1)
			}
			case Trap(vect: Int) => {
				//This could be used to imitate system calls in the future
			}
		}
	}
	
}