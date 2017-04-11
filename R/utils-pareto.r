
#https://rosettacode.org/wiki/Nth_root#R
nthroot <- function(A, n, tol=sqrt(.Machine$double.eps))
{
  ifelse(A < 1, x0 <- A * n, x0 <- A / n)
  repeat
  {
    x1 <- ((n-1)*x0 + A / x0^(n-1))/n
    if(abs(x1 - x0) > tol) x0 <- x1 else break
  }
  x1
}


cst_qpareto <- function(p, scale, shape) {
  return(scale / nthroot(1-p,shape))
}

pareto_get_k <- function(p, scale, shape, n, r) {
  return(ceil(cst_qpareto(nthroot(p, n-1), scale, shape) * r))
}