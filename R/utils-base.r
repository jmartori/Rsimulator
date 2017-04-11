# Basic utility functions/macros that can be included to most scripts.

vector2string <- function(...) paste(..., collapse="")
ln <- function(...) log(..., base=exp(1))
count <- function(...) length(...)
printf <- function(...) cat(sprintf(...))

list_pos <- function(v, n=1) lapply(v, '[[', n)


is.empty <- function (v) length(v) == 0


which.closest <- function (v, val=0) {
  return(sapply(val, function(n) which.min(abs(v-n))))
}

shift <- function (v, rot=1) {
	
	len <- length(v)

	if (rot > 0) {
		v <- c(rep.int(v[1], rot), v[1:(len-rot)])
	} else if (rot < 0) {
		rot <- abs(rot)
		v <- c(v[(rot+1):len], rep.int(v[len], rot))
	}

	return (v)
}

closest <- function(v, w, range=seq(0,1,0.1) )
{
	return(
		sapply(range, function(p) {
			return (which.closest(v,p) - which.closest(w,p))
		})
	)
}


monotonize <- function ( v )
{
  max_val <- 1
  pos <- which( v > 1 )
  if (is.empty(pos)) {
    pos <- 0
  } else {
    pos <- min(pos)
  }
  if (pos != 0){
    v[pos:length(v)] <- rep.int(1, length(v) - pos + 1)
  }
  return (v)
}

max_n <- function(v, n=1)
{
  m <- length(v)
  if (n > m) n <- m
  
  return (sort(v, decreasing=TRUE)[1:n])
}

min_n <- function(v, n=1)
{
  m <- length(v)
  if (n > m) n <- m
  
  return (sort(v, decreasing=FALSE)[1:n])
}

even <- function(n=1) return(2*(1:n))
odd <- function(n=1) return((2*(0:(n-1)))+1)