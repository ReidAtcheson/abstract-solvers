#ifndef __FD_H_
#define __FD_H_

#include <stdint.h>


#define ORDER 2
#define HALF 1
typedef int64_t index_t;
typedef double real_t;

typedef struct{
  index_t nx;
  index_t ny;
  index_t nz;
  real_t* data;
} fd_grid_t;


void* make(index_t nx,index_t ny,index_t nz);
void unmake(void* _in);
void fill(void* _in,real_t val);

void addv(void* _left,void* _right,void* _out);
void subv(void* _left,void* _right,void* _out);
void mulv(void* _left,real_t alpha,void* _out);
void divv(void* _left,real_t alpha,void* _out);
real_t dot(void* _left,void* _right);
void fd(void* _in,void* _out);

#endif
