extern int readImg(const char* filename,unsigned char** img,int *wp,int *hp);
extern int readBmp(const char* filename,unsigned char** img,int *wp,int *hp);
extern int readPng(const char* filename,unsigned char** img,int *wp,int *hp);
extern int readJpeg(const char* filename,unsigned char** img,int *wp,int *hp);
extern void freeImg(unsigned char* img);
extern int writeBmp(const char* file,unsigned char* img,int w,int h);
extern int writePng(const char* file,unsigned char* img,int w,int h);
extern int writeJpeg(const char* file,unsigned char* img,int w,int h);
