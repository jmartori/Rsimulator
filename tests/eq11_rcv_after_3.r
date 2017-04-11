# Equation 11
#
#

source("./tests/utils-test.r")

eq_rcv_after_3 <- function(l=1, t=0, d=0) 
{
	return (3 * exp(-l * ( t + d) )/8)
}

sim_rcv_after_3 <- function(l=1, t=0, d=0, iter=2500) 
{
	v <- sapply(t, function(taux) {
		v_rcv1 <- rexp(iter,rate=l) + d + taux
		v_rcv2 <- apply(matrix(rexp(iter*2,rate=l), ncol=2), 1, sum) + v_rcv1
		v_snd  <- rexp(iter,rate=l)
		return (length(v_snd[(v_snd > v_rcv1) & (v_snd < v_rcv2)])/length(v_snd))
	})
	
	return (v)
}

plot_eq_sim <- function( v_l=1, d=0, v_t)
{
	plot(v_t, get_y_axis(v_t),type="n")
	for (l in v_l) {
		v <- eq_rcv_after_3(t=v_t, l=l)
		w <- sim_rcv_after_3(t=v_t, l=l)

		points(v, type="l", col="blue")
		points(w, type="l", col="green")

		err <- rmse_compute_error(v,w)
		print(err)
	}
}




