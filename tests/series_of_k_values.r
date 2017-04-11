#### Comparting the probabilities
## Current way (the wrong way?)

v1 <- sapply(0:24, function(i) {
	exp_snd_vis(t = t + (i/r), n=5, l=1)
})

v2 <- sapply(0:96, function(i) {
	exp_rcv_vis(t=t + (i*0.8/r), n=5, l=1)
})

p1 <- prod(c(v1,v2))



## Alternative 

p2 <- prod(sapply(0:96, function(i) {
	if ( i%%5 == 0) {
		v <- exp_snd_vis(t = t + (i*0.2/r), n=5, l=1)
	} else {
		v <- exp_rcv_vis(t=t + (i*0.8/r), n=5, l=1)
	}
	return (v)
}))

#### Comparing t
## Current way (the wrong way?)
k <- 24
n <- 5
t <- 5
r <- 25

v1 <- sapply(0:k, function(i) {
	t + (i/r)
})

v2 <- sapply(1:(k*(n-1)), function(i) {
	t + (i/(5*r))
})

t1 <- (c(v1,v2))



## Alternative 
t2 <- sapply(0:(k*n), function(i) {
	if ( i%%5 == 0) {
		v <- t + (i/(5*r))
	} else {
		v <- t + (i/(5*r))
	}
	return (v)
})