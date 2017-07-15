// Program: ImageBMP.java
// helq: Elkin Cruz
// Date: 2012-04-24

package utilities;

import java.io.*;

// Clase que genera objetos a partir de una matriz de int que representan a
// pixeles en formato BMP
public class ImageBMP
{
   // Header of BMP
   static final String header = "BM";
   private int size;
   static final int reserved = 0;
   static final int offBMP = 54;

   // Header DIB
   static final int sizeDIB = 40;
   private int width;
   private int height;
   static final short plane = 1;
   static final short bitsPixel = 24;
   static final int compress = 0;
   private int sizeData;
   static final int resolution = 2835;
   static final int nColors = 0;
   static final int iColors = 0;

   // Data
   private byte [] imageData;

   // Clase que guarda la imagen una vez ha sido normalizada a 4 bytes y su
   // tamaño original de ancho (w) y largo (h)
   private static class ImageData{
      public byte [] i;
      public int w, h;
      public ImageData( byte [] ii, int ww, int hh ) { i = ii; w = ww; h = hh; }
   }

   // Contructor de la clase
   private ImageBMP( ImageData iD )
   {
      imageData = iD.i;

      sizeData = imageData.length;
      size = offBMP + sizeData;
      width = iD.w;
      height = iD.h;
   }

   // Constructor de la clase que recibe una matriz de enteros, cada entero
   // representa un color en RGB
   public ImageBMP( int [][] a, int zoomDegr, int zoom, boolean borde ) {
      this( intsTo3Ints(a), zoomDegr, zoom, borde );
   }

   // Constructor de la clase que recibe una matriz de arrays de enteros, cada
   // array de int tiene tamaño 3 en una matriz cuadrada. El valor 0 del array
   // corresponde con Red, el 0 y 1 con Green y Blue respectivamente
   public ImageBMP( int [][][] a, int zoomDegr, int zoom, boolean borde ) {
      this(toBMPData( zoomDegrImage(zoomImage(a, zoom), zoomDegr),
                      zoomDegr, zoomDegr>1 ? borde : false )
          );
   }

   // Toma una matriz de arrays de enteros y la convierte en un un array de
   // enteros simple con cada elemento con un valor de 0 a 255 (un byte).
   // Devueleve el tamaño original de la matriz, la matriz normalizada en un
   // objeto ImageData.
   static ImageData toBMPData( int [][][] a, int zoomDegr, boolean borde )
   {
      int b = borde ? zoomDegr : 0;
      int h = a.length - b*2; // Heigth
      int w = (a[0].length - b*2)*3;
      // Dejando todas las filas como múltiplos de cuatro
      int wD = w - w%4; wD = wD==w ? wD : wD+4;

      byte [] toReturn = new byte[h*wD];

      w = a[0].length-b*2;

      for (int i=h-1; i>=0; i--)
         for (int j=0; j<w; j++)
         {
            toReturn[i*wD+3*j]   = (byte) a[h-i-1+b][j+b][2]; // RED
            toReturn[i*wD+3*j+1] = (byte) a[h-i-1+b][j+b][1]; // GREEN
            toReturn[i*wD+3*j+2] = (byte) a[h-i-1+b][j+b][0]; // BLUE
         }

      ImageData c = new ImageData(toReturn, a.length-2*b, a[0].length-2*b  );
      return c;
   }

   // Método que toma una matriz de arrays de y lo convierte en una matriz con
   // longitudes multiplos de zoom, aumenta la matriz en zoom veces por cada lado.
   static int [][][] zoomImage( int [][][] a, int zoom )
   {
      if(zoom<=1) return a;

      int h = a.length;
      int w = a[0].length;
      int hB = h*zoom;
      int wB = w*zoom;

      int [][][] b = new int[hB][wB][3];

      // pixel por pixel
      for (int i=0; i<h; i++)
         for (int j=0; j<w; j++)
            // Copia el color de un pixel en un cuadrado de pixeles
            for (int k=0; k<zoom; k++)
               for (int l=0; l<zoom; l++)
                  b[i*zoom+k][j*zoom+l] = a[i][j];

      return b;
   }

   // Crea una copia de la matriz de arrays y difumina el resultado, usar con
   // precausión, al aunmentar bastantes veces la imagen original resulta en una
   // imagen borrosa y poco agradable.
   static int [][][] zoomDegrImage( int [][][] a, int zoom )
   {
      if (zoom<=1) return a;

      int h = a.length;
      int w = a[0].length;
      int hB = (h+1)*zoom+1;
      int wB = (w+1)*zoom+1;

      int [][][] b = new int[hB][wB][3];

      // Copiando imagen a en b con espaciado
      for (int i=0; i<h; i++)
         for (int j=0; j<w; j++)
            b[(i+1)*zoom][(j+1)*zoom] = a[i][j];

      int [] white = {0xff, 0xff, 0xff};
      // Poniendo borde blanco
      for (int i=0; i<hB; i++)   b[i][0] = b[i][wB-1] = white;
      for (int j=1; j<wB-1; j++) b[0][j] = b[hB-1][j] = white;

      // Procedimiento de relleno de pixeles

      // Rellena los espacios verticales entre pixeles copiados
      int tmp;
      for (int i=0; i<h+1; i++)
         for (int j=0; j<w; j++){
            tmp = (j+1)*zoom;
            for (int k=1; k<zoom; k++)
               b[i*zoom+k][tmp] = 
                        meanPixels(b[i*zoom][tmp], b[(i+1)*zoom][tmp], k, zoom);
         }

      // Rellena los espacios horizontales entre pixeles copiados
      for (int i=0; i<h; i++){
         tmp = (i+1)*zoom;
         for (int j=0; j<w+1; j++)
            for (int k=1; k<zoom; k++)
               b[tmp][j*zoom+k] = 
                        meanPixels(b[tmp][j*zoom], b[tmp][(j+1)*zoom], k, zoom);
      }

      // Rellena la zona cuadrada restante, que se encuentra entre los espacios
      // horizontales y verticales ya rellenados
      int tmp2;
      for (int i=0; i<h+1; i++)
         for (int j=0; j<w+1; j++)
            for (int k=1; k<zoom; k++)
            {
               tmp = i*zoom;
               for (int l=1; l<zoom; l++)
               {
                  tmp2 = j*zoom;
                  b[tmp+k][tmp2+l] =
                           meanFourPixels( b[tmp][tmp2+l],    b[tmp+zoom][tmp2+l],
                                           b[i*zoom+k][tmp2], b[tmp+k][tmp2+zoom],
                                           k, l, zoom );
               }
            }

      return b;
   }

   // Toma dos arrays de enteros con tres elementos cada uno y halla la esperanza
   // de el las primeras posiciones (Red), luego de las segundas (Green) y de las
   // terceras (Blue).
   static int [] meanPixels( int [] a, int [] b, int k, int zoom ){
      int [] c = new int[3];
      for(int i=0; i<3; i++) c[i] = ((zoom-k)*a[i] + k*b[i])/zoom;
      return c;
   }

   // Realiza el procedimientod ed meanPixels con dos parejas y luego halla la
   // media de cada posición.
   static int [] meanFourPixels( int [] a1, int [] a2, int [] b1, int [] b2,
                                 int k, int l, int zoom )
   {
      int [] a = meanPixels(a1, a2, k, zoom),
             b = meanPixels(b1, b2, l, zoom),
             c = new int[3];

      for(int i=0; i<3; i++) c[i] = (a[i] + b[i])/2;
      return c;
   }

   // Toma una matriz de enteros y la convierte en una matriz de arrays de enteros
   static int [][][] intsTo3Ints( int [][] a )
   {
      int h = a.length; // Heigth
      int w = a[0].length;

      int [][][] b = new int[h][w][3];
      int tmp;
      for (int i=0; i<h; i++)
         for (int j=0; j<w; j++)
         {
            tmp = a[i][j];
            b[i][j][2] = tmp & 0xff; // BLUE
            b[i][j][1] = tmp>>8 & 0xff; // GREEN
            b[i][j][0] = tmp>>16 & 0xff; // RED
         }
      return b;
   }

   // Crea un array de tres enteros de tamaño byte de un único entero de 32bits
   // que debe representar a tres enteros de tamaño byte.
   static byte [] intToBytes(long v, int n)
   {
      byte [] a = new byte[n];
      for(int i=0; i<n; i++)
         a[i] = (byte)(0xff & (v >> 8*i));
      return a;
   }

   // Guarda la imagen en formato BMP
   public void save( String fn )
   {
      fn += ".bmp";
      try{
         FileOutputStream f = new FileOutputStream(fn);
         DataOutputStream o = new DataOutputStream(f);

         o.writeBytes(header);
         o.write( intToBytes(size,4) );
         o.write( intToBytes(reserved,4) );
         o.write( intToBytes(offBMP,4) );

         o.write( intToBytes(sizeDIB,4) );
         o.write( intToBytes(height,4) );
         o.write( intToBytes(width,4) );
         o.write( intToBytes(plane,2) );
         o.write( intToBytes(bitsPixel,2) );
         o.write( intToBytes(compress,4) );
         o.write( intToBytes(sizeData,4) );
         o.write( intToBytes(resolution,4) );
         o.write( intToBytes(resolution,4) );
         o.write( intToBytes(nColors,4) );
         o.write( intToBytes(iColors,4) );
         o.write( imageData );

         f.close();
      }
      catch(Exception e) {
         System.err.println("Error: Se presento el error\n" + e.getMessage());
      }

      System.out.println("Imagen " + fn + " correctamente guardada");
   }

   // Prueba de la clase
   public static void main(String [] args)
   {
      int [][][] a = {{ {0,0,255}, {0,255,0} },
                      { {255,0,0}, {255,255,255} }};
      int [][] b = {{ 0x0000ff, 0x00ff00 },
                    { 0xff0000, 0xffffff }};

      ImageBMP p1 = new ImageBMP(a, 3, 5, false);
      p1.save("prueba1");

      ImageBMP p2 = new ImageBMP(b, 2, 15, true);
      p2.save("prueba2");
   }
}
