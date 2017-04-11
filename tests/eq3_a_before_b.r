# Equation 3
#
#

source("./tests/utils-test.r")


eq_a_before_b <- function(t=0, l=1)
{
	return (1 - 0.5*exp(-l*t) )
}

sim_a_before_b <- function(t=0, l=1, iter=2500)
{
	v <- sapply(t, function(taux) {
		v_rcv <- rexp(iter,rate=l) + taux
		v_snd <- rexp(iter,rate=l)

		return (length(v_snd[(v_snd < v_rcv)])/length(v_snd))
	})
	
	return (v)
}