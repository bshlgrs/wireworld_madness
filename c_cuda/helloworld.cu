#include <stdio.h>
#define N 512

__global__ void add(int *a, int *b, int *c) {
  c[blockIdx.x] = a[blockIdx.x] + b[blockIdx.x];
}

int main(void) {
  int *a, *b, *c;
  int *d_a, *d_b, *d_c;
  int size = N * sizeof(int);

  cudaMalloc((void **)&d_a, size);
  cudaMalloc((void **)&d_b, size);
  cudaMalloc((void **)&d_c, size);

  a = (int *)malloc(size);
  // random_ints(a, N);
  b = (int *)malloc(size);
  // random_ints(b, N);
  c = (int *)malloc(size);

  a[0] = 10;
  b[0] = 7;

  cudaMemcpy(d_a, a, size, cudaMemcpyHostToDevice);
  cudaMemcpy(d_b, b, size, cudaMemcpyHostToDevice);


  add<<<N,1>>>(d_a, d_b, d_c);

  cudaMemcpy(c, d_c, size, cudaMemcpyDeviceToHost);

  free(a); free(b); free(c);
  cudaFree(d_a); cudaFree(d_b); cudaFree(d_c);

  printf("Hello World! %d %d %d, %d %d %d\n", a[0], b[0], c[0], a[1], b[1], c[1]);
  return 0;
}