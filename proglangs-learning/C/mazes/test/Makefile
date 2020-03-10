#Makefile para construir Paging (Punto 2, Lab 4 - Sistemas Operativos )

CC = gcc
CFLAGS = -c -std=c11
OBJS = paging.o

Paging : $(OBJS) gnuplot_i.o
	$(CC) $(OBJS) -Wall gnuplot_i/gnuplot_i.o -o Paging

paging.o : paging.h gnuplot_i/src/gnuplot_i.h mt19937ar/mt19937ar.h paging.c
	$(CC) $(CFLAGS) -Wall paging.c

gnuplot_i.o :
	cd gnuplot_i; make

clean : 
	rm -rf Paging $(OBJS) *.dat
