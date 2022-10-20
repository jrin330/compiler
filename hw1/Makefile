#
# makefile for TINY
# Borland C Version
# K. Louden 2/3/98
#

CC = gcc

CFLAGS = 

OBJS = main.o util.o scan.o 
#parse.o symtab.o analyze.o code.o cgen.o

hw1_binary: $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o hw1_binary

main.o: MAIN.C GLOBALS.H UTIL.H SCAN.H PARSE.H ANALYZE.H CGEN.H
	$(CC) $(CFLAGS) -c MAIN.C

util.o: UTIL.C UTIL.H GLOBALS.H
	$(CC) $(CFLAGS) -c UTIL.C

scan.o: SCAN.C SCAN.H UTIL.H GLOBALS.H
	$(CC) $(CFLAGS) -c SCAN.C

#parse.o: PARSE.C PARSE.H SCAN.H GLOBALS.H UTIL.H
#	$(CC) $(CFLAGS) -c PARSE.C

#symtab.o: SYMTAB.C SYMTAB.H
#	$(CC) $(CFLAGS) -c SYMTAB.C

#analyze.o: ANALYZE.C GLOBALS.H SYMTAB.H ANALYZE.H
#	$(CC) $(CFLAGS) -c ANALYZE.C

#code.o: CODE.C CODE.H GLOBALS.H
#	$(CC) $(CFLAGS) -c CODE.C

#cgen.o: CGEN.C GLOBALS.H SYMTAB.H CODE.H CGEN.H
#	$(CC) $(CFLAGS) -c CGEN.C

clean:
	-del hw1_binary
	-del TM.EXE
	-del MAIN.o
	-del UTIL.o
	-del SCAN.o
#	-del parse.o
#	-del symtab.o
#	-del analyze.o
#	-del code.o
#	-del cgen.obj
	-del tm.o

tm.exe: TM.C
	$(CC) $(CFLAGS) -etm TM.C

tm: tm.exe

all: hw1_binary tm

