/*
 * CS:APP Data Lab
 *
 * <Please put your name and userid here>
 *
 * bits.c - Source file with your solutions to the Lab.
 *          This is the file you will hand in to your instructor.
 *
 * WARNING: Do not include the <stdio.h> header; it confuses the dlc
 * compiler. You can still use printf for debugging without including
 * <stdio.h>, although you might get a compiler warning. In general,
 * it's not good practice to ignore compiler warnings, but in this
 * case it's OK.
 */

#if 0
/*
 * Instructions to Students:
 *
 * STEP 1: Read the following instructions carefully.
 */

You will provide your solution to the Data Lab by
editing the collection of functions in this source file.

INTEGER CODING RULES:
 
  Replace the "return" statement in each function with one
  or more lines of C code that implements the function. Your code 
  must conform to the following style:
 
  int Funct(arg1, arg2, ...) {
      /* brief description of how your implementation works */
      int var1 = Expr1;
      ...
      int varM = ExprM;

      varJ = ExprJ;
      ...
      varN = ExprN;
      return ExprR;
  }

  Each "Expr" is an expression using ONLY the following:
  1. Integer constants 0 through 255 (0xFF), inclusive. You are
      not allowed to use big constants such as 0xffffffff.
  2. Function arguments and local variables (no global variables).
  3. Unary integer operations ! ~
  4. Binary integer operations & ^ | + << >>
    
  Some of the problems restrict the set of allowed operators even further.
  Each "Expr" may consist of multiple operators. You are not restricted to
  one operator per line.

  You are expressly forbidden to:
  1. Use any control constructs such as if, do, while, for, switch, etc.
  2. Define or use any macros.
  3. Define any additional functions in this file.
  4. Call any functions.
  5. Use any other operations, such as &&, ||, -, or ?:
  6. Use any form of casting.
  7. Use any data type other than int.  This implies that you
     cannot use arrays, structs, or unions.

 
  You may assume that your machine:
  1. Uses 2s complement, 32-bit representations of integers.
  2. Performs right shifts arithmetically.
  3. Has unpredictable behavior when shifting if the shift amount
     is less than 0 or greater than 31.


EXAMPLES OF ACCEPTABLE CODING STYLE:
  /*
   * pow2plus1 - returns 2^x + 1, where 0 <= x <= 31
   */
  int pow2plus1(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     return (1 << x) + 1;
  }

  /*
   * pow2plus4 - returns 2^x + 4, where 0 <= x <= 31
   */
  int pow2plus4(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     int result = (1 << x);
     result += 4;
     return result;
  }

FLOATING POINT CODING RULES

For the problems that require you to implement floating-point operations,
the coding rules are less strict.  You are allowed to use looping and
conditional control.  You are allowed to use both ints and unsigneds.
You can use arbitrary integer and unsigned constants. You can use any arithmetic,
logical, or comparison operations on int or unsigned data.

You are expressly forbidden to:
  1. Define or use any macros.
  2. Define any additional functions in this file.
  3. Call any functions.
  4. Use any form of casting.
  5. Use any data type other than int or unsigned.  This means that you
     cannot use arrays, structs, or unions.
  6. Use any floating point data types, operations, or constants.


NOTES:
  1. Use the dlc (data lab checker) compiler (described in the handout) to 
     check the legality of your solutions.
  2. Each function has a maximum number of operations (integer, logical,
     or comparison) that you are allowed to use for your implementation
     of the function.  The max operator count is checked by dlc.
     Note that assignment ('=') is not counted; you may use as many of
     these as you want without penalty.
  3. Use the btest test harness to check your functions for correctness.
  4. Use the BDD checker to formally verify your functions
  5. The maximum number of ops for each function is given in the
     header comment for each function. If there are any inconsistencies 
     between the maximum ops in the writeup and in this file, consider
     this file the authoritative source.

/*
 * STEP 2: Modify the following functions according the coding rules.
 * 
 *   IMPORTANT. TO AVOID GRADING SURPRISES:
 *   1. Use the dlc compiler to check that your solutions conform
 *      to the coding rules.
 *   2. Use the BDD checker to formally verify that your solutions produce 
 *      the correct answers.
 */

#endif
/* Copyright (C) 1991-2022 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <https://www.gnu.org/licenses/>.  */
/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */
/* glibc's intent is to support the IEC 559 math functionality, real
   and complex.  If the GCC (4.9 and later) predefined macros
   specifying compiler intent are available, use them to determine
   whether the overall intent is to support these features; otherwise,
   presume an older compiler has intent to support these features and
   define these macros by default.  */
/* wchar_t uses Unicode 10.0.0.  Version 10.0 of the Unicode Standard is
   synchronized with ISO/IEC 10646:2017, fifth edition, plus
   the following additions from Amendment 1 to the fifth edition:
   - 56 emoji characters
   - 285 hentaigana
   - 3 additional Zanabazar Square characters */
// 1

void	printBinary	( int x )
{
	for ( int i = 31; i >= 0; i--)
	{
		int b = (x >> i);
		b = b & 1;

		if (i % 4 == 3)
		{
			printf(" ");
		}
		printf("%d",b);
	}
	printf("\n");

}



/*
 * bitAnd - x&y using only ~ and |
 *   Example: bitAnd(6, 5) = 4
 *   Legal ops: ~ |
 *   Max ops: 8
 *   Rating: 1
 */
/*
 * Khai Le
 * We can do the "and" operation using only negation and disjunction by
 * negating `x & y` twice and then using De'Morgan's Law to replace the "and"
 * into an "or". See the following boolean algebra:
 * x & y = ~~(x & y)
 *       = ~((~x) | (~y)) [De'Morgan's Law]
 *
 * The above shows `x & y` is equivalent to `~((~x) | (~y))` which only uses
 * negation and disjunction.
 * */
int bitAnd(int x, int y)
{
   int result = ~(~x | ~y);

   return result;
}
/*
 * tmin - return minimum two's complement integer
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 4
 *   Rating: 1
 */
/*
 * Joel Singh
 * The minimum two's complement integer is 0x80000000, or in binary is 1
 * followed by 31 zeroes. This is because in two's complement, every bit
 * represents a positive value except for the leftmost bit which has a negative
 * weight. Through simple logic, the minimum two's complement integer must be a
 * binary number where only the negative bit is 1.
 *
 * Because we are restricted in our size of constants, we can't simply return
 * 0x80000000. We can instead take 1, and shift it 31 to the left to yield
 * 0x80000000.
 * */
int tmin(void)
{
   return 1 << 31;
}
/* Khai Le
 * bitMatch - Create mask indicating which bits in x match those in y
 *            using only ~ and &
 *   Example: bitMatch(0x7, 0xE) = 0x6
 *   Legal ops: ~ & |
 *   Max ops: 14
 *   Rating: 1
 */
int bitMatch(int x, int y)
{

   // Find which bit couple has different values first.
   // Look at the description for XOR function for explanation.
   int mask = ~(x & y) & ~(~x & ~y);

   // Then find the one that actually match.
   mask = ~mask;

   return mask;
}
/*
 * bitXor - x^y using only ~ and &
 *   Example: bitXor(4, 5) = 1
 *   Legal ops: ~ &
 *   Max ops: 14
 *   Rating: 1
 */
/*
Aurora Hodar
Xor is recreated with ~ and & by first finding two expressions.
The first, ~(x & y) returns false only when x and y are both true, while
the second ~(~x & ~y) does the opposite, and returns false
only when x and y are both false. Thus, combining them
with & returns true only when only one of x or y are true*/
int bitXor(int x, int y)
{
   return ~(x & y) & ~(~x & ~y);
}
// 2
/*
 * implication - return x -> y in propositional logic - 0 for false, 1
 * for true
 *   Example: implication(1,1) = 1
 *            implication(1,0) = 0
 *   Legal ops: ! ~ ^ |
 *   Max ops: 5
 *   Rating: 2
 *
 * Cole Clodgo
 * x -> y is the same as (not x) or y
 * x^1 flips a 1 to a 0 or a 0 to a 1,
 * which gives us not x
 */
int implication(int x, int y)
{
   return (x^1) | y;
}
/*
 * negate - return -x
 *   Example: negate(1) = -1.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 5
 *   Rating: 2
 */
int negate(int x)
{
   /* Uses two's complement: -x == ~x + 1 */
   int flipped = ~x;
   int result = flipped + 1;
   return result;
   return 2;
}
/* Khai Le
 * isPositive - return 1 if x > 0, return 0 otherwise
 *   Example: isPositive(-1) = 0.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 8
 *   Rating: 2
 */
int isPositive(int x)
{

   // We have two checks here: to see if x is not negative, and see if x is not 0.
   // For x not negative check, we have '!((x >> 31) & 1)'. Basically,
   // Since the types have 4 bytes. which is 32 bits, I use '>>' to move all
   // the way to the first bit, which is the sign that dictate
   // x to be negative or not. Then I do an '&' operation of that and 1.
   // If it's negative (aka 1), the result will be 1 (since 1 announce the number
   // as negative), and 0 for vice versa. The '!' operator will check result, and
   // return 1 if it's not a negative, and vice versa. As for '!!x', it checks
   // for the x=0 case specifically. The first '!' essentially convert x to 0 and
   // 1 form. Basically, if x is not 0, return 0, and vice versa. The second '!'
   // check the result, and check true if the result tell x is not 0, and vice
   // versa. Finally, we use '&' to check the two conditions. If both are 1, that
   // means x is positive, and we return 1. If not, return 0.
   int result = (!((x >> 31) & 1)) & (!!x);

   return result;
}
/*
 * isPallindrome - Return 1 if bit pattern in x is equal to its mirror image
 *   Example: isPallindrome(0x01234567E6AC2480) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 45
 *   Rating: 4
 */

// NOTE: this one is not done! I haven't put the printBinary (cause I don't have it) function in.
// If sopmeone can do that that would be great.
//
// NOTE: JOEL: Commented out the `printBinary`s for now so the program compiles
int isPallindrome(int x)
{
   int hi16, lo16;
   int mask;
   int lo8, lo4, lo2, lo1;
   x = 0xCA87E153;

   // Step 1: isolate the upper 16 bits and shift right.
   mask = (0xFF << 8) | 0xFF;
   hi16 = (x >> 16) & mask;
   printf("hi16 = ");
   // printBinary(hi16);

   // Step 2: isolate the lower 16 bits.
   lo16 = x & mask;
   printf("lo16 = ");
   // printBinary(lo16);

   // Step 3: swap groups of 8 bits in lower half.
   mask = 0xFF;

   lo8 = (lo16 & mask) << 8;
   lo8 = lo8 | ((lo16 >> 8) & mask);
   printf("lo8 = ");
   // printBinary(lo8);

   // Step 4: swap 2 groups of 4 bits in lower half.
   mask = (0xF << 8) | 0xF;

   lo4 = (lo8 & mask) << 4;
   lo4 = lo4 | ((lo8 >> 4) & mask);
   printf("lo4 = ");
   // printBinary(lo4);

   return 2;
}
// 3
// 4
/*
 * bang - Compute !x without using !
 *   Examples: bang(3) = 0, bang(0) = 1
 *   Legal ops: ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 4
 *
 * Cole Clodgo, Khai Lee, Joel Singh
 * Finds the absolute value of x and makes it negative
 * If x is 0, will still be 0
 * If x is anything else, will have a leading 1 (seen as -1 because of two's complement)
 * Can use a NOT operator and add 1 to get 1 is nonzero, will still be 0 if x is 0
 * Finally, use XOR with 1 to flip to the proper output
 */
int bang(int x)
{
	// Get the absolute value of x
	int y;
  	y = x >> 31;
    x = (x + (x >> 31)) ^ y;
    
    // make the absolute value negative
    // - if x is 0, will still be 0
    // - otherwise x will have a leading 1
    x = ~x + 1;
    
    // when x is nonzero, x >> 31 gives -1
    // XOR with 1 flips the bit so we get 1 when x=0, and 0 otherwise
    return (~(x >> 31) + 1) ^ 1;
}
/*
 * absVal - absolute value of x
 *   Example: absVal(-1) = 1.
 *   You may assume -TMax <= x <= TMax
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 10
 *   Rating: 4
 */
/*
Aurora Hodar, Isabella Rivera, Cole Clodgo
absVal returns the absolute value of an inputted integer x, by
isolating the leftmost bit of x and adding it to 0xFFFFFFFF to create 
a desired mask of 1s or 0s, matching that leftmost bit. 
We then return the result of XOR against that mask and either x (if positive),
or x - 1 (if negative).
This is achieved by adding x + (x >> 31), which again results in integer overflow and subtracts 1,
which is the equivalent of flipping the bits 
*/
int absVal(int x)
{
   int y, var;
   
   // Step 1: Isolate the leftmost bit of x (which determines if x is positive or negative),
   // and negate it so that y = 1 if x is negative or y = 0 if x is positive
   y = (x >> 31);

   // Step 2: Make a var of repeating 1s
   // var = ~0;
   
   // Step 3: If y == 1, this results in integer overflow and y will be a mask of repeating 0s.
   // Otherwise, y = var and can be used as a mask of repeating 1s.
   // y = y + var;
   
   // Step 4: if x is negative, then x >> 31 is 11...1, which when added to x,
   // causes integer overflow and results in x - 1 (in two's complement)
   // 1110 (-2) becomes 1101 (-3)
   // If x is positive, this addition changes nothing.
   // Then there is an XOR flip against the y mask to 
   // return the absolute value of x.
   return (x + (x >> 31)) ^ y;
}

   // printBinary(x);
   // printBinary((x >> 31));
   // printBinary(x + (x >> 31));

// 1110 = -2
// flip
// 0001 + 1 = 0010 (2)

// 1110 + 1 = 1111
// 0000
