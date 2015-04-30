#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdint.h>

//#define PNG_DEBUG 3

#ifndef WIN32
#include <unistd.h>
#endif  
#include <png.h>
#include <jpeglib.h>

#include "img.h"

#define C(img,x,y,z) ((img)+((y)*w+(x))*3)[(z)]
#define CRGB ((img)+(y*w+x)*3)
#define R (CRGB[0])
#define G (CRGB[1])
#define B (CRGB[2])
#define SET4(dat) {int32_t li=(int32_t)(dat);memcpy(data+pos,&li ,4);pos+=4;}
#define SET2(dat) {int16_t si=(int16_t)(dat);memcpy(data+pos,&si ,2);pos+=2;}
#define SET1(dat) {unsigned char ci=(unsigned char)(dat);data[pos++]=ci;}

void
tryReadBmp(const char* filename,int* pw,int* ph){
  int16_t bpp;
  unsigned char *begin,*pos;
  unsigned char *parret;
  int offset;
  unsigned char *data=NULL;
  long size=0;
  FILE* file=fopen(filename,"rb");
  unsigned char ppos;
  unsigned short int p;
  unsigned int** img;
  int w,h;
  int x,y;
  if(file==NULL)
    return;
  
  fseek(file,0,SEEK_END);
  size=ftell(file);
  data=(unsigned char*)malloc(size);
  
  fseek(file,0,SEEK_SET);
  fread(data,size,1,file);

  w=*(int32_t*)(data+18);
  h=*(int32_t*)(data+22);
  fclose(file);
}

void
tryReadPng(const char* file,int* pw,int* ph){
  char header[8];// 8 is the maximum size that can be checked
  png_structp  png_ptr;
  png_infop    info_ptr;
  int w,h;
  FILE *fp = fopen(file, "rb");
  int color_type;
  int bit_depth;
  int    r;
  int    hs;
  int    ts;
  unsigned char*  t;
  unsigned char* p;
  unsigned char** hp;
  int i;

  fread(header, 1, 8, fp);
  assert(png_sig_cmp((png_byte*)header, 0, 8)==0);

  png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  assert(png_ptr!=NULL);

  info_ptr = png_create_info_struct(png_ptr);
  assert(info_ptr!=NULL);

  if (setjmp(png_jmpbuf(png_ptr))){
    fprintf(stderr,"[read_png_file] Error during init_io");
    abort();
  }

  png_init_io(png_ptr, fp);
  png_set_sig_bytes(png_ptr, 8);

  png_read_info(png_ptr, info_ptr);

  w=(int)info_ptr->width;
  h=(int)info_ptr->height;
  png_destroy_read_struct(&png_ptr, &info_ptr,(png_infopp)NULL);

  fclose(fp);
}

void
tryReadJpeg(const char* file,int* pw,int* ph){
  struct jpeg_decompress_struct cinfo;
  struct jpeg_error_mgr jerr;
  JSAMPARRAY img;
  int width;
  int height;
  FILE* infile;
  int w,h;
  int i,j,c;

  cinfo.err = jpeg_std_error( &jerr );
  jpeg_create_decompress( &cinfo );

  infile = fopen(file, "rb" );
  jpeg_stdio_src( &cinfo, infile );
  jpeg_read_header( &cinfo, TRUE );
  width = cinfo.output_width;
  height = cinfo.output_height;
  jpeg_destroy_decompress( &cinfo );
  fclose( infile );


  w=(int)width;
  h=(int)height;
}


void
readBmp(const char* filename,unsigned char* img){
  int16_t bpp;
  unsigned char *begin,*pos;
  unsigned char *parret;
  int offset;
  unsigned char *data=NULL;
  long size=0;
  FILE* file=fopen(filename,"rb");
  unsigned char ppos;
  unsigned short int p;
  int w,h;
  int x,y;
  //  int32_t headersize=0;
  if(file==NULL)
    return;
  
  fseek(file,0,SEEK_END);
  size=ftell(file);
  data=(unsigned char*)malloc(size);
  
  fseek(file,0,SEEK_SET);
  fread(data,size,1,file);

  offset=*(int32_t*)(data+10);
  w=*(int32_t*)(data+18);
  h=*(int32_t*)(data+22);
  bpp=*(int16_t*)(data+28);
  begin=(unsigned char*)data+offset;
  parret=(unsigned char*)data+54;
  
  
  for(y=0;y<h;y++){
    pos=begin+
      (w * bpp/8+
       (4-(w * bpp/8)%4)%4
       )*(h-y-1);
    for(x=0;x<w;x++){
      switch(bpp){
      case 1:
	ppos=(*pos>>(7-x%8))&0x1;
	B=*(parret+(ppos*4));
	G=*(parret+(ppos*4)+1);
	R=*(parret+(ppos*4)+2);
	if(x%8==7)
	  pos++;
	break;
      case 4:
	ppos=(*pos>>((1-x%2)*4))&0xf;
	B=*(parret+(ppos*4));
	G=*(parret+(ppos*4)+1);
	R=*(parret+(ppos*4)+2);
	if(x%2==1)
	  pos++;
	break;
      case 8:
	ppos=*pos;
	B=*(parret+(ppos*4));
	G=*(parret+(ppos*4)+1);
	R=*(parret+(ppos*4)+2);
	pos++;
	break;
      case 16:
	p=*(unsigned short int*)pos;
	B=0x1f&p;
	G=(0x3e0&p)>>5;
	R=(0x7c00&p)>>10;
	pos+=2;
	break;
      case 24:
	B=*pos++;
	G=*pos++;
	R=*pos++;
	break;
      case 32:
	B=*pos++;
	G=*pos++;
	R=*pos++;
	pos++;
	break;
      default:
	printf("Does not support bpp:%d\n",bpp);
	exit(1);
      }
    }
  }
  free(data);
  fclose(file);
}

void
readPng(const char* file,unsigned char* img){
  char header[8];// 8 is the maximum size that can be checked
  png_structp  png_ptr;
  png_infop    info_ptr;
  int w,h;
  FILE *fp = fopen(file, "rb");
  int color_type;
  int bit_depth;
  int    r;
  int    hs;
  int    ts;
  unsigned char*  t;
  unsigned char* p;
  unsigned char** hp;
  int i;

  fread(header, 1, 8, fp);
  assert(png_sig_cmp((png_byte*)header, 0, 8)==0);

  png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  assert(png_ptr!=NULL);

  info_ptr = png_create_info_struct(png_ptr);
  assert(info_ptr!=NULL);

  if (setjmp(png_jmpbuf(png_ptr))){
    fprintf(stderr,"[read_png_file] Error during init_io");
    abort();
  }

  png_init_io(png_ptr, fp);
  png_set_sig_bytes(png_ptr, 8);

  png_read_info(png_ptr, info_ptr);

  w=(int)info_ptr->width;
  h=(int)info_ptr->height;

  color_type = info_ptr->color_type;
  bit_depth  = info_ptr->bit_depth;

  png_set_interlace_handling(png_ptr);
  if (color_type & PNG_COLOR_MASK_ALPHA)
    png_set_strip_alpha(png_ptr);

  if (color_type == PNG_COLOR_TYPE_PALETTE)
    png_set_palette_to_rgb(png_ptr);

  if (color_type == PNG_COLOR_TYPE_GRAY &&
      bit_depth < 8) png_set_gray_1_2_4_to_8(png_ptr);

  png_read_update_info(png_ptr, info_ptr);

  if (setjmp(png_jmpbuf(png_ptr))){
    fprintf(stderr,"[read_png_file] Error during read_image");
    abort();
  }

  r  = (int)info_ptr->rowbytes;
  hs = (int)sizeof(png_bytep)*h;
  ts = r * h;
  t  = (unsigned char*) malloc(hs+ts);
  p   = t+hs;
  hp = (unsigned char**)t;
  assert(t!=NULL);

  for(i=0;i<h;i++)
    hp[i]=p+r*i;
  
  png_read_image(png_ptr, (png_byte**)t);

  for(i=0;i<h*w;i++){
    C(img,i%w,i/w,0)=p[i*3];
    C(img,i%w,i/w,1)=p[i*3+1];
    C(img,i%w,i/w,2)=p[i*3+2];
  }

  free(t);
  
  png_destroy_read_struct(&png_ptr, &info_ptr,(png_infopp)NULL);

  fclose(fp);
}

void
readJpeg(const char* file,unsigned char* rimg){
  struct jpeg_decompress_struct cinfo;
  struct jpeg_error_mgr jerr;
  JSAMPARRAY img;
  int width;
  int height;
  FILE* infile;
  int w,h;
  int i,j,c;

  cinfo.err = jpeg_std_error( &jerr );
  jpeg_create_decompress( &cinfo );

  infile = fopen(file, "rb" );

  jpeg_stdio_src( &cinfo, infile );

  jpeg_read_header( &cinfo, TRUE );
  jpeg_start_decompress( &cinfo );

  width = cinfo.output_width;
  height = cinfo.output_height;

  img = (JSAMPARRAY)malloc( sizeof( JSAMPROW ) * height );
  for (i = 0; i < height; i++ ) 
    img[i] = (JSAMPROW)malloc( sizeof( JSAMPLE )* 3 * width );


  while( cinfo.output_scanline < cinfo.output_height ) {
    jpeg_read_scanlines( &cinfo,
			 img + cinfo.output_scanline,
			 cinfo.output_height - cinfo.output_scanline
			 );
  }

  jpeg_finish_decompress( &cinfo );
  jpeg_destroy_decompress( &cinfo );
  fclose( infile );


  w=(int)width;
  h=(int)height;

  for (i = 0; i < height; i++ )
    for (j = 0; j < width; j++ ) 
      for(c=0;c<3;c++)
	C(rimg,j,i,c)=img[i][j * 3 + c];

  for (i = 0; i < height; i++ )
    free( img[i] );
  free( img );
}

void
writeBmp(const char* file,unsigned char* img,int w,int h){
  unsigned char* data=NULL;
  int16_t bpp=24;
  int32_t fsize=54+(w *bpp/8+(4-(w * bpp/8)%4)%4)*h;
  int pos=0;
  int diff;
  int x,y,j;
  data=(unsigned char*)malloc(fsize);
  memset(data,0,fsize);
  
  
  SET1('B');
  SET1('M');
  SET4(fsize);
  SET2(0);
  SET2(0);
  SET4(54);
  SET4(40);
  SET4(w);
  SET4(h);
  SET2(1);
  SET2(bpp);
  SET4(0);
  SET4(fsize-54);
  SET4(0);
  SET4(0);
  SET4(0);
  SET4(0);

  diff=(4-(w*(bpp/8))%4)%4;
  for(y=h-1;y>=0;y--){
    for(x=0;x<w;x++){
      SET1(B);
      SET1(G);
      SET1(R);
    }
    for(j=0;j<diff;j++){
      SET1(0);
    }
  }
  if(fsize!=pos){
    printf("fsize:%d != pos:%d\n",(int)fsize,(int)pos);
    exit(1);
  }

  FILE* f=fopen(file,"wb");
  fwrite(data,fsize,1,f);
  fclose(f);
  free(data);
}


void
writePng(const char* file,unsigned char* vimg,int w,int h){
  png_structp  png_ptr;
  png_infop    info_ptr;
  FILE *fp = fopen(file, "wb");
  int color_type;
  int bit_depth;
  int    r;
  int    hs;
  int    ts;
  unsigned char*  t;
  unsigned char* p;
  unsigned char** hp;
  int i;
  assert(fp!=NULL);

  png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  assert(png_ptr!=NULL);

  info_ptr = png_create_info_struct(png_ptr);
  assert(info_ptr!=NULL);

  if (setjmp(png_jmpbuf(png_ptr))){
    fprintf(stderr,"[write_png_file] Error during init_io");
    abort();
  }

  png_init_io(png_ptr, fp);

  if (setjmp(png_jmpbuf(png_ptr))){
    fprintf(stderr,"[write_png_file] Error during writing header");
    abort();
  }

  bit_depth=8;
  color_type=PNG_COLOR_TYPE_RGB;
  
  png_set_IHDR(png_ptr, info_ptr, w, h,
	       bit_depth, color_type, PNG_INTERLACE_NONE,
	       PNG_COMPRESSION_TYPE_BASE, PNG_FILTER_TYPE_BASE);

  png_write_info(png_ptr, info_ptr);


  if (setjmp(png_jmpbuf(png_ptr))){
    fprintf(stderr,"[write_png_file] Error during writing bytes");
    abort();
  }

  r  = (int)info_ptr->rowbytes;
  hs = (int)sizeof(png_bytep)  * h;
  ts = r * h;
  t  = (unsigned char*) malloc(hs+ts);
  p  = t+hs;
  hp = (unsigned char**)t;
  assert(t!=NULL);

  for(i=0;i<h;i++)
    hp[i]=p+r*i;
  
  for(i=0;i<h*w;i++){
     p[i*3]  =(unsigned char)C(vimg,i%w,i/w,0);
     p[i*3+1]=(unsigned char)C(vimg,i%w,i/w,1);
     p[i*3+2]=(unsigned char)C(vimg,i%w,i/w,2);
  }

  png_write_image(png_ptr,(png_byte**)t);


  if (setjmp(png_jmpbuf(png_ptr))){
    fprintf(stderr,"[write_png_file] Error during end of write");
    abort();
  }

  png_write_end(png_ptr, NULL);

  
  free(t);

  png_destroy_write_struct(&png_ptr, &info_ptr);

  fclose(fp);
}

void
writeJpeg(const char* file,unsigned char* vimg,int w,int h){
  struct jpeg_compress_struct cinfo;
  struct jpeg_error_mgr jerr;
  FILE *outfile;
  JSAMPARRAY img;
  int i, j;

  img = (JSAMPARRAY)malloc( sizeof( JSAMPROW ) * h );
  for ( i = 0; i < h; i++ ) {
    img[i] = (JSAMPROW)malloc( sizeof( JSAMPLE ) * 3 * w );
    for ( j = 0; j < w; j++ ) {
      img[i][ j * 3 + 0 ] = (unsigned char)C(vimg,j,i,0);
      img[i][ j * 3 + 1 ] = (unsigned char)C(vimg,j,i,1);
      img[i][ j * 3 + 2 ] = (unsigned char)C(vimg,j,i,2);
    }
  }

  cinfo.err = jpeg_std_error( &jerr );
  jpeg_create_compress( &cinfo );

  outfile = fopen( file, "wb" );
  jpeg_stdio_dest( &cinfo, outfile );

  cinfo.image_width = w;
  cinfo.image_height = h;
  cinfo.input_components = 3;
  cinfo.in_color_space = JCS_RGB;

  jpeg_set_defaults( &cinfo );
  jpeg_start_compress( &cinfo, TRUE );

  jpeg_write_scanlines( &cinfo, img, h );
  jpeg_finish_compress( &cinfo );

  jpeg_destroy_compress( &cinfo );

  fclose( outfile );
  for ( i = 0; i < h; i++ )
    free( img[i] );
  free( img );
}


