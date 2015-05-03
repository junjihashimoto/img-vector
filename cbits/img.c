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

int
readImg(const char* file,unsigned char** imgp, int* wp, int* hp){
  FILE *fp;
  char buf[4];
  int r;
  fp=fopen(file,"rb");
  memset(buf,0,sizeof(buf));
  r=fread(buf,1,4,fp);
  if(r!=4){
    fclose(fp);
    return -1;
  }
  fclose(fp);

  if(buf[0]=='B' &&
     buf[1]=='M')
    return readBmp(file,imgp,wp,hp);
  else if(buf[1]=='P' &&
	  buf[2]=='N' &&
	  buf[3]=='G')
    return readPng(file,imgp,wp,hp);
  else if(buf[0]==(char)0xff &&
	  buf[1]==(char)0xd8 &&
	  buf[2]==(char)0xff &&
	  buf[3]==(char)0xe0 )
    return readJpeg(file,imgp,wp,hp);
  else
    return -1;
}

void
freeImg(unsigned char* imgp){
  if(imgp!=NULL)
    free(imgp);
}

int
readBmp(const char* filename,unsigned char** imgp,int* wp,int* hp){
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
  unsigned char* img;
  int r;
  //  int32_t headersize=0;
  if(file==NULL)
    return -1;
  
  fseek(file,0,SEEK_END);
  size=ftell(file);
  data=(unsigned char*)malloc(size);
  
  fseek(file,0,SEEK_SET);
  r=fread(data,size,1,file);
  if(r!=1){
    fclose(file);
    free(data);
    return -1;
  }

  offset=*(int32_t*)(data+10);
  *wp=w=*(int32_t*)(data+18);
  *hp=h=*(int32_t*)(data+22);
  *imgp=img=(unsigned char*)malloc(sizeof(unsigned char)*3*w*h);
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
	if (data!=NULL)
	  free(data);
	if (img!=NULL)
	  free(img);
	fclose(file);
	return -2;
      }
    }
  }
  free(data);
  fclose(file);
  return 0;
}

int
readPng(const char* file,unsigned char** imgp,int* wp,int* hp){
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
  unsigned char*  p;
  unsigned char** hpp;
  int i;
  unsigned char* img;

  r=fread(header, 1, 8, fp);
  if(r!=8){
    fclose(fp);
    return -1;
  }
  assert(png_sig_cmp((png_byte*)header, 0, 8)==0);

  png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  if(png_ptr==NULL)
    return -1;

  info_ptr = png_create_info_struct(png_ptr);
  if(info_ptr==NULL)
    return -1;

  if (setjmp(png_jmpbuf(png_ptr))){
    return -1;
  }

  png_init_io(png_ptr, fp);
  png_set_sig_bytes(png_ptr, 8);

  png_read_info(png_ptr, info_ptr);

  *wp=w=(int)info_ptr->width;
  *hp=h=(int)info_ptr->height;
  *imgp=img=(unsigned char*)malloc(sizeof(unsigned char)*3*w*h);

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
    free(img);
    return -1;
  }

  r  = (int)info_ptr->rowbytes;
  hs = (int)sizeof(png_bytep)*h;
  ts = r * h;
  t  = (unsigned char*) malloc(hs+ts);
  p   = t+hs;
  hpp = (unsigned char**)t;

  for(i=0;i<h;i++)
    hpp[i]=p+r*i;
  
  png_read_image(png_ptr, (png_byte**)t);

  for(i=0;i<h*w;i++){
    C(img,i%w,i/w,0)=p[i*3];
    C(img,i%w,i/w,1)=p[i*3+1];
    C(img,i%w,i/w,2)=p[i*3+2];
  }

  free(t);
  png_destroy_read_struct(&png_ptr, &info_ptr,(png_infopp)NULL);
  fclose(fp);
  return 0;
}

int
readJpeg(const char* file,unsigned char** imgp,int* wp,int* hp){
  struct jpeg_decompress_struct cinfo;
  struct jpeg_error_mgr jerr;
  JSAMPARRAY jimg;
  int width;
  int height;
  FILE* infile;
  int w,h;
  int i,j,c;
  unsigned char* img;

  cinfo.err = jpeg_std_error( &jerr );
  jpeg_create_decompress( &cinfo );

  infile = fopen(file, "rb" );

  jpeg_stdio_src( &cinfo, infile );

  jpeg_read_header( &cinfo, TRUE );
  jpeg_start_decompress( &cinfo );

  *wp = w = cinfo.output_width;
  *hp = h = cinfo.output_height;
  *imgp=img=(unsigned char*)malloc(sizeof(unsigned char)*3*w*h);

  jimg = (JSAMPARRAY)malloc( sizeof( JSAMPROW ) * h );
  for (i = 0; i < h; i++ ) 
    jimg[i] = (JSAMPROW)malloc( sizeof( JSAMPLE )* 3 * w );


  while( cinfo.output_scanline < cinfo.output_height ) {
    jpeg_read_scanlines( &cinfo,
			 jimg + cinfo.output_scanline,
			 cinfo.output_height - cinfo.output_scanline
			 );
  }

  jpeg_finish_decompress( &cinfo );
  jpeg_destroy_decompress( &cinfo );
  fclose( infile );


  for (i = 0; i < h; i++ )
    for (j = 0; j < w; j++ ) 
      for(c=0;c<3;c++)
	C(img,j,i,c)=jimg[i][j * 3 + c];

  for (i = 0; i < h; i++ )
    free( jimg[i] );
  free( jimg );
  return 0;
}

int
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
    return -1;
  }

  FILE* f=fopen(file,"wb");
  fwrite(data,fsize,1,f);
  fclose(f);
  free(data);
  return 0;
}


int
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
  if(fp==NULL)
    return -1;

  png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  if(png_ptr==NULL)
    return -1;

  info_ptr = png_create_info_struct(png_ptr);
  if(info_ptr==NULL)
    return -1;

  if (setjmp(png_jmpbuf(png_ptr))){
    return -2;
  }

  png_init_io(png_ptr, fp);

  if (setjmp(png_jmpbuf(png_ptr))){
    return -2;
  }

  bit_depth=8;
  color_type=PNG_COLOR_TYPE_RGB;
  
  png_set_IHDR(png_ptr, info_ptr, w, h,
	       bit_depth, color_type, PNG_INTERLACE_NONE,
	       PNG_COMPRESSION_TYPE_BASE, PNG_FILTER_TYPE_BASE);

  png_write_info(png_ptr, info_ptr);


  if (setjmp(png_jmpbuf(png_ptr))){
    return -2;
  }

  r  = (int)info_ptr->rowbytes;
  hs = (int)sizeof(png_bytep)  * h;
  ts = r * h;
  t  = (unsigned char*) malloc(hs+ts);
  p  = t+hs;
  hp = (unsigned char**)t;
  if(t==NULL)
    return -1;

  for(i=0;i<h;i++)
    hp[i]=p+r*i;
  
  for(i=0;i<h*w;i++){
     p[i*3]  =(unsigned char)C(vimg,i%w,i/w,0);
     p[i*3+1]=(unsigned char)C(vimg,i%w,i/w,1);
     p[i*3+2]=(unsigned char)C(vimg,i%w,i/w,2);
  }

  png_write_image(png_ptr,(png_byte**)t);


  if (setjmp(png_jmpbuf(png_ptr))){
    return -1;
  }

  png_write_end(png_ptr, NULL);
  free(t);

  png_destroy_write_struct(&png_ptr, &info_ptr);

  fclose(fp);
  return 0;
}

int
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
  return 0;
}


