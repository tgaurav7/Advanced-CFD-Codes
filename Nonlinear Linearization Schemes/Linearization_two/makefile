#
# Template for `main'
#
# This makefile is designed to compile and link Fortran code 
# for the MME 9710 Code Assign 1
# 
# Notes 1) All the source files and makefile should be in
#          the same directory.
#       2) This version allows for the use of dbx to control 
#          program during execution for debugging. 
#       3) The executable code is called `main'.
#       4) The mainline should be in the file main.f
#       5) To compile your code, type: make

# Set Fortran compiler flags:
FFLAGS = -c -C -g

# List of object files to be linked:
LIST = main.o\
bndct.o\
coeff.o\
difphi.o\
grdgeo.o\
inital.o\
input.o\
makgrd.o\
null.o\
out1d.o\
resid.o\
srct.o\
tdma.o\

# Rule for making an executable called `main'.
main : $(LIST)  
	gfortran $(LIST)  -o main

# Rule for creating an object file from its dependent fortran file.
%.o : %.f  
	gfortran $(FFLAGS) $*.f -o $*.o

