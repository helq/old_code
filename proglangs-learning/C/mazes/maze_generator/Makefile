CC = gcc
CFLAGS = -std=c99 -Wall -O3

maze : utils.h mt19937ar/mt19937ar.h camino_principal/DFS.h maze_generator.c
	$(CC) $(CFLAGS) maze_generator.c -o maze

clean : 
	rm maze
