#include <stdio.h>
#include <time.h>
#include "utils.h"
#include "camino_principal/DFS.h"

bool proc_matrix(casilla **, int);

#define FAIL     0
#define ALL_GOOD 1
int main()
{
    init_genrand( time(NULL) );

    int iter = 50;
    int puntos = 1;

    int fails = 0;

    for(int i=0; i<iter; i++)
    {
        // inicializando matriz
        casilla **matrix = (casilla **) malloc( ALTO*sizeof(casilla *) );
        casilla *a = (casilla *) calloc( ANCHO*ALTO, sizeof(casilla)) ;
        for(int i=0; i<ALTO; i++)
            matrix[i] = a+ANCHO*i;
        for(int i=0; i<ALTO; i++)
            for(int j=0; j<ANCHO; j++)
                create_walls1( matrix[i][j] );

        // "creando maze"
        bool maze = proc_matrix(matrix, puntos);
        if(maze==FAIL) {
            printf("ops\n");
            fails++;
        }
        else {
            printf("time is: %#lx\n", time(NULL));
            printMazePaths(matrix, ANCHO, ALTO);
        }

        // liberando recursos
        free(a);
        free(matrix);
    }
    printf("FAILS: %d\n", fails);

    return 0;
}

bool proc_matrix(casilla **matrix, int puntos)
{
    // abriendo paredes de entrada y salida
    open_left1( matrix[0][0] );
    open_right1( matrix[ALTO-1][ANCHO-1] );

    pointxy point1 = START;
    for(int i=0; i<puntos; i++) {
        // generate random point in laberinth
        uint32_t tmp = genrand_int32();
        pointxy point2 = {
              .x = extract_ancho(tmp)
            , .y = extract_alto(tmp)
        };

        // create rute from point1 to point2
        if( create_rute_DFS(matrix, point1, point2) )
            return FAIL; // abort

        point1 = point2;
    }
    // create rute from point1 to END
    pointxy pointEND = END;
    if( create_rute_DFS(matrix, point1, pointEND) )
        return FAIL; // abort

    mark(matrix, pointEND);

    return ALL_GOOD;
}

