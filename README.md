# abstract solvers

A series of abstract solvers that abstract over the type of vectors, real numbers, complex numbers,
and operators.

# Motivation

Traditionally Krylov solvers can accept a black-box linear operator function as input,
but usually require you to use a predefined vector type and one of either 32 bit float or 64 bit floats.
For experimenting with different data layouts, parallelism strategies, or even more exotic vector types, this can
significantly limit the Krylov solver's applicability.

This work was originally motivated by two blog posts I wrote showing how to compute spectral information of highly
abstract operators, although I have modified my approach somewhat. Those posts can be found below:

* [Post 1](http://www.reidatcheson.com/numerical%20analysis/linear%20algebra/ocaml/functional%20programming/2016/10/14/abstract-numerical-linear-algebra.html)
* [Post 2](http://www.reidatcheson.com/numerical%20analysis/linear%20algebra/ocaml/functional%20programming/2016/12/22/abstract-linear-revisited.html)



# Goals

I aim to have useful Krylov iterative methods which can apply to highly tuned code written in C and also to
highly abstract code written in Ocaml.



# Current examples

in examples/ I have three examples


1. Hilbert space of OCaml floating point arrays
2. Hilbert space of of smooth functions with L^2 inner product
3. Hilbert space of custom C data types (a finite difference grid) interfaced via ctypes library


All of these have corresponding tests in tests/ where I solve a linear system expressed in each
respective hilbert space. Right now I have to ensure the operators are symmetric and positive
definite because I only have Richardson iteration and Conjugate Gradients, but by incorporating
the [Owl project](https://github.com/ryanrhymes/owl) I will be able to define GMRES as well.



# Roadmap


## Linear Solvers
[ x ] - Richardson Solver
[ x ] - Conjugate Gradient solver
[   ] - Symmetrically preconditioned Conjugate Gradient
[   ] - GMRES 
[   ] - BiCGstab
[   ] - Flexible conjugate gradient
[   ] - Flexible GMRES


## Nonlinear Solvers
[   ] - Newton solver for nonlinear systems
[   ] - Optimizers


## Eigenvalue Solvers
[   ] - Power iteration
[   ] - Arnoldi iterations
[   ] - Lanczos iterations
