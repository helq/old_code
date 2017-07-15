#define GOOD_PATH 1
#define BAD_PATH  0

#define markDFS(matrix, p) (matrix[p.y][p.x] |= 0x2)
#define unmarkDFS(n) (n &= (~0x0)^0x2)
#define is_markedDFS(c) (c & 0x2)
#define is_markedDFSAndNormal(c) (c & 0x3)

pointxy move(pointxy, int);
bool is_valid_moveDFS(casilla **, pointxy, int);
void open_wall(casilla **, pointxy, int);
int DFS(casilla **, pointxy, pointxy);

// return 0 if ok, else 1
int create_rute_DFS(casilla **matrix, pointxy start, pointxy end)
{
    // limpiando maze para ejecutar DFS
    for(int i=0; i<ALTO; i++)
        for(int j=0; j<ANCHO; j++)
            unmarkDFS(matrix[i][j]);

    // ejecutando DFS de start a end, si falla retornar error
    if(DFS(matrix, start, end) == BAD_PATH)
        return 1;

    return 0;
}

int DFS(casilla **matrix, pointxy start, pointxy end) {
    if(start.x==end.x && start.y==end.y)
        return GOOD_PATH;

    markDFS(matrix, start);

    // generating random moves
    int moves[4] = {0, 1, 2, 3};
    for(int i = 4; i>1; i--) {
        int m = genrand_int32() % i;
        swap(int, moves[i-1], moves[m]);
    }

    for(int i=0; i<4; i++) {
        // getting a new move
        int mov = moves[i];

        if( is_valid_moveDFS(matrix, start, mov) ) {

            pointxy new_move = move(start, mov);
            if( DFS(matrix, new_move, end) ) {

                mark(matrix, start);
                // abriendo la pared seleccionada
                open_wall(matrix, start, mov);
                open_wall(matrix, new_move, inverse_mov(mov));
                
                return GOOD_PATH;
            }

        }

        mov = (mov+1) & 0x3;
    }

    return BAD_PATH;
}

bool is_valid_moveDFS(casilla **matrix, pointxy p, int mov) {
    int x = p.x;
    int y = p.y;

    switch (mov) {
        case UP:
            if(y==0       || is_markedDFSAndNormal(matrix[y-1][x]))
                return false;
            break;
        case LEFT:
            if(x==0       || is_markedDFSAndNormal(matrix[y][x-1]))
                return false;
            break;
        case RIGHT:
            if(x==ANCHO-1 || is_markedDFSAndNormal(matrix[y][x+1]))
                return false;
            break;
        case DOWN:
            if(y==ALTO-1  || is_markedDFSAndNormal(matrix[y+1][x]))
                return false;
            break;
        default:
            printf("WTF!");
            return false;
    }
    return true;
}

pointxy move(pointxy p, int mov) {
    pointxy q = p;

    switch (mov) {
        case UP:    (q.y) --; break;
        case LEFT:  (q.x) --; break;
        case RIGHT: (q.x) ++; break;
        case DOWN:  (q.y) ++; break;
    }
    return q;
}

void open_wall(casilla **matrix, pointxy p, int mov) {
    switch (mov) {
        case UP:    open_up   ( matrix, p ); break;
        case LEFT:  open_left ( matrix, p ); break;
        case RIGHT: open_right( matrix, p ); break;
        case DOWN:  open_down ( matrix, p ); break;
    }
}
