extern void tryReadBmp(const char* filename,int* pw,int* ph);
extern void tryReadPng(const char* filename,int* pw,int* ph);
extern void tryReadJpeg(const char* filename,int* pw,int* ph);
extern void readBmp(const char* filename,unsigned char* img);
extern void readPng(const char* filename,unsigned char* img);
extern void readJpeg(const char* filename,unsigned char* img);
extern void writeBmp(const char* file,unsigned char* img,int w,int h);
extern void writePng(const char* file,unsigned char* img,int w,int h);
extern void writeJpeg(const char* file,unsigned char* img,int w,int h);
