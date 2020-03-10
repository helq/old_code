colorLine = 'rgb(0,0,0)'
svgLine = lambda ((x1, y1), (x2, y2)): '\t<line x1="'+ str(x1) +'" y1="'+ str(y1) +'" x2="'+ str(x2) +'" y2="'+ str(y2) +'" style="stroke:'+ colorLine +';stroke-width:1" />\n'

def walls(node, i, j):
    (u, d, l, r) = node
    ws = []
    if u: ws.append( ((j    *10, i    *10), ((j+1)*10, i    *10)) )
    if d: ws.append( ((j    *10, (i+1)*10), ((j+1)*10, (i+1)*10)) )
    if l: ws.append( ((j    *10, i    *10), (j    *10, (i+1)*10)) )
    if r: ws.append( (((j+1)*10, i    *10), ((j+1)*10, (i+1)*10)) )
    return ws

def saveMatrixMaze(matrix, name):
    f = open(name, 'w')
    f.write('<svg xmlns="http://www.w3.org/2000/svg" version="1.1">\n')
    for i in range(len(matrix)):
        for j in range(len(matrix[0])):
            f.writelines( map(svgLine, walls(matrix[i][j], i, j)) )
    f.write('</svg>\n')
    f.close()

def test():
    m = [[(True, False, True, True),(True, True, True, False)]
        ,[(False, True, True, False),(True, True, False, True)]]
    saveMatrixMaze(m, "test.svg")

test()
