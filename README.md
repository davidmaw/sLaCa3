sLaCa3
======

An internal DSL for Scala implementing LC3, an educational assembly language conceived by Yale Patt at the University of Texas at Austin. It is described in the book "Introduction to Computing Systems by Patt and Patel". The framework for this project was taken from a DSL for BASIC, provided here: https://github.com/fogus/baysick

The two example sLaCa3 programs perform simple computation on 16 bit values.

countingConsecutivesTest counts the highest number of consecutive zeroes in the binary number stored in memory location x3100, and then stores the count into memory location x3101.

count01sTest counts the number of occurences of "01" in the binary number stored in memory location x3100, and then stores the count into memory location x3101.
