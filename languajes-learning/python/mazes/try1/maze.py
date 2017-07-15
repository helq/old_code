from random import randint
from mazeSVG import saveMatrixMaze

class Node:
    def __init__(self):
        self.edges = []
    def addEdge(self, weight, edge):
        self.edges.append( (weight, edge) )

high = 3
width = 5

maze_matrix = [[Node() for j in range(width)] for i in range(high)]

for i in range(high):
    for j in range(width-1):
        maze_matrix[i][j]  .addEdge( randint(0,(1<<16)-1), maze_matrix[i][j+1] )
        maze_matrix[i][j+1].addEdge( randint(0,(1<<16)-1), maze_matrix[i][j] )

for i in range(high-1):
    for j in range(width):
        maze_matrix[i]  [j].addEdge( randint(0,(1<<16)-1), maze_matrix[i+1][j] )
        maze_matrix[i+1][j].addEdge( randint(0,(1<<16)-1), maze_matrix[i][j] )

