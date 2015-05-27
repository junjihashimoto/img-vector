#include <string.h>
#include <stdio.h>
#include <fftw3.h>


fftw_complex*
fft1d(fftw_complex* in_org,int n,int direction){
  int size= sizeof(fftw_complex) * n;
  fftw_plan p;
  fftw_complex* in=(fftw_complex*) fftw_malloc(size);
  fftw_complex* out=(fftw_complex*) fftw_malloc(size);
  int i;
  double* outd;
  
  if(out==NULL||in==NULL){
    if(out!=NULL)
      fftw_free(out);
    if(in!=NULL)
      fftw_free(in);
    return NULL;
  }
  memcpy(in,in_org,size);
  p = fftw_plan_dft_1d(n, in, out, direction ? FFTW_FORWARD : FFTW_BACKWARD, FFTW_ESTIMATE);
  fftw_execute(p);
  fftw_destroy_plan(p);  
  fftw_free(in);
  if(!direction)
    for(i=0;i<n;i++){
      outd=(double*) (out+i);
      outd[0]/=n;
      outd[1]/=n;
    }
  return out;
}

fftw_complex*
fft2d(fftw_complex* in_org,int m,int n,int direction){
  int size= sizeof(fftw_complex) * m * n;
  fftw_plan p;
  fftw_complex* in=(fftw_complex*) fftw_malloc(size);
  fftw_complex* out=(fftw_complex*) fftw_malloc(size);
  int i;
  double* outd;
  if(out==NULL||in==NULL){
    if(out!=NULL)
      fftw_free(out);
    if(in!=NULL)
      fftw_free(in);
    return NULL;
  }
  memcpy(in,in_org,size);
  p = fftw_plan_dft_2d(m, n, in, out, direction ? FFTW_FORWARD : FFTW_BACKWARD, FFTW_ESTIMATE);
  fftw_execute(p);
  fftw_destroy_plan(p);  
  fftw_free(in);
  if(!direction)
    for(i=0;i<m*n;i++){
      outd=(double*) (out+i);
      outd[0]/=m*n;
      outd[1]/=m*n;
    }
  return out;
}


fftw_complex*
fft3d(fftw_complex* in_org,int l,int m,int n,int direction){
  int size= sizeof(fftw_complex) * l * m * n;
  fftw_plan p;
  fftw_complex* in=(fftw_complex*) fftw_malloc(size);
  fftw_complex* out=(fftw_complex*) fftw_malloc(size);
  int i;
  double* outd;
  if(out==NULL||in==NULL){
    if(out!=NULL)
      fftw_free(out);
    if(in!=NULL)
      fftw_free(in);
    return NULL;
  }
  memcpy(in,in_org,size);
  p = fftw_plan_dft_3d(l, m, n, in, out, direction ? FFTW_FORWARD : FFTW_BACKWARD, FFTW_ESTIMATE);
  fftw_execute(p);
  fftw_destroy_plan(p);  
  fftw_free(in);
  if(!direction)
    for(i=0;i<l*m*n;i++){
      outd=(double*) (out+i);
      outd[0]/=l*m*n;
      outd[1]/=l*m*n;
    }
  return out;
}

