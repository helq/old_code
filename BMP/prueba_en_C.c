#include <stdio.h>
#include <stdint.h>

struct bmpfile_header {
  uint32_t size;
  uint16_t reserved;
  uint32_t bmp_offset;
};

struct bmpInfoHeader{
  uint32_t header_sz;
  int32_t width;
  int32_t height;
  uint16_t nplanes;
  uint16_t bitspp;
  uint32_t compress_type;
  uint32_t bmp_bytesz;
  int32_t hres;
  int32_t vres;
  uint32_t ncolors;
  uint32_t nimpcolors;
};

void saveImage(struct bmpfile_header header, struct bmpInfoHeader info, FILE* fp)
{
   char headerString[] = "BM";
   fprintf(fp, headerString);
   fwrite(&header, sizeof(struct bmpfile_header), 1, fp);
   fwrite(&info, sizeof(struct bmpInfoHeader), 1, fp);
}

int main()
{
   FILE *fp;
   char filename[] = "prueba.txt";

   uint8_t a[2][2][3] = {{ {0,0xff,0xff}, {0xff,0,0} },
                         { {0xff,0xff,0xff}, {0,0,0} }};

   struct bmpfile_header header;
   header.reserved = 0;
   header.bmp_offset = 0x36;

   struct bmpInfoHeader info;
   info.header_sz = 0;
   info.width;
   info.height;
   info.nplanes;
   info.bitspp;
   info.compress_type;
   info.bmp_bytesz;
   info.hres;
   info.vres;
   info.ncolors;
   info.nimpcolors;

   header.size = info.header_sz + info.bmp_bytesz;

   fp = fopen(filename, "wb");
   saveImage(header, info, fp);
   fclose(fp);

   return 0;
}
