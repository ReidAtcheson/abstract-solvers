#include <stdlib.h>
#include "fd.h"

void* make(index_t nx,index_t ny,index_t nz){
  fd_grid_t* out = malloc(sizeof(fd_grid_t));
  out->nx=nx;
  out->ny=ny;
  out->nz=nz;
  out->data = malloc(nx*ny*nz*sizeof(real_t));
  return (void*)out;
}

void unmake(void* _in){
  fd_grid_t* in = (fd_grid_t*)_in;
  free(in->data);
  in->nx=-1;
  in->ny=-1;
  in->nz=-1;
  free(in);
}
void fill(void* _in,real_t val){
  fd_grid_t* in = (fd_grid_t*)_in;
  real_t* inf = in->data;
  index_t nx=in->nx;
  index_t ny=in->ny;
  index_t nz=in->nz;
  for(index_t i=0;i<nx*ny*nz;i++) inf[i]=val;
}

void addv(void* _left,void* _right,void* _out){
  fd_grid_t* left = (fd_grid_t*)_left;
  fd_grid_t* right = (fd_grid_t*)_right;
  fd_grid_t* out = (fd_grid_t*)_out;
  real_t* lf = left->data;
  real_t* rf = right->data;
  real_t* of = out->data;

  index_t nx=left->nx;
  index_t ny=left->ny;
  index_t nz=left->nz;
  for(index_t i=0;i<nx*ny*nz;i++){
    of[i]=lf[i]+rf[i];
  }
}

void subv(void* _left,void* _right,void* _out){
  fd_grid_t* left = (fd_grid_t*)_left;
  fd_grid_t* right = (fd_grid_t*)_right;
  fd_grid_t* out = (fd_grid_t*)_out;
  real_t* lf = left->data;
  real_t* rf = right->data;
  real_t* of = out->data;

  index_t nx=left->nx;
  index_t ny=left->ny;
  index_t nz=left->nz;
  for(index_t i=0;i<nx*ny*nz;i++){
    of[i]=lf[i]-rf[i];
  }
}
void mulv(void* _left,real_t alpha,void* _out){
  fd_grid_t* left = (fd_grid_t*)_left;
  fd_grid_t* out = (fd_grid_t*)_out;
  real_t* lf = left->data;
  real_t* of = out->data;

  index_t nx=left->nx;
  index_t ny=left->ny;
  index_t nz=left->nz;
  for(index_t i=0;i<nx*ny*nz;i++){
    of[i]=lf[i]*alpha;
  }

}
void divv(void* _left,real_t alpha,void* _out){
  fd_grid_t* left = (fd_grid_t*)_left;
  fd_grid_t* out = (fd_grid_t*)_out;
  real_t* lf = left->data;
  real_t* of = out->data;

  index_t nx=left->nx;
  index_t ny=left->ny;
  index_t nz=left->nz;
  for(index_t i=0;i<nx*ny*nz;i++){
    of[i]=lf[i]/alpha;
  }


}
real_t dot(void* _left,void* _right){
  fd_grid_t* left = (fd_grid_t*)_left;
  fd_grid_t* right = (fd_grid_t*)_right;
  real_t* lf = left->data;
  real_t* rf = right->data;

  real_t out=0.0;
  index_t nx=left->nx;
  index_t ny=left->ny;
  index_t nz=left->nz;
  for(index_t i=0;i<nx*ny*nz;i++){
    out+=lf[i]*rf[i];
  }
  return out;
}


void fd(void* _in,void* _out){
  fd_grid_t* in = (fd_grid_t*)_in;
  fd_grid_t* out = (fd_grid_t*)_out;
  real_t* inf = in->data;
  real_t* of = out->data;

  index_t nx=in->nx;
  index_t ny=in->ny;
  index_t nz=in->nz;
  real_t dx=0.1;
  real_t dy=0.1;
  real_t dz=0.1;

  real_t _c[ORDER+1]={-1.0,2.0,-1.0};
  real_t invdx2 = 1.0 / (dx*dx);
  real_t invdy2 = 1.0 / (dy*dy);
  real_t invdz2 = 1.0 / (dz*dz);
  real_t* c=_c+1;


#define id(ix,iy,iz) ( (ix) + nx*( (iy) + ny*(iz)) )
  for(index_t ix=0;ix<nx;ix++){
    for(index_t iy=0;iy<ny;iy++){ 
      for(index_t iz=0;iz<nz;iz++){
        of[id(ix,iy,iz)]=0.0;
        for(index_t i=-HALF;i<=HALF;i++){
          if(ix+i>=0 && ix+i<nx){
            of[id(ix,iy,iz)] += c[i]*invdx2*inf[id(ix+i,iy,iz)];
          }
          if(iy+i>=0 && iy+i<ny){
            of[id(ix,iy,iz)] += c[i]*invdy2*inf[id(ix,iy+i,iz)];
          }
          if(iz+i>=0 && iz+i<nz){
            of[id(ix,iy,iz)] += c[i]*invdz2*inf[id(ix,iy,iz+i)];
          }
        }
      }
    }
  }
}

