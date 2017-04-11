# We test an equation given by Wiebe Pestman in http://www.researchgate.net/post/What_is_CDF_for_PmaxX2_Xn-X1_t_with_exponential_distributions

# t is time
# l is lambda
# n is number of replicas
# v_x is for the plot values.


eq_visibility_n_event <- function(t=0, l=1, n=5)
{
	return((exp(l*t)/n) * (1 - (1 - exp(-l*t))^n))
}


sim_visibility_n_event <- function(t=0, l=1, n=5, iter=2500)
{

	return(sapply(t, function(taux)
	{
		x_max <- apply(matrix(rexp((n-1)*iter, l), ncol=(n-1)), 1, max )
			
		x_1 <- rexp(iter, l)
	
		a <- x_max - x_1 
		return (length(a[a<=taux])/iter)
	}))
}

plot_eq_sim <- function(v_x, a, b, m=NULL)
{
	m <- cbind(m, matrix(a, ncol=1), matrix(b, ncol=1))
	
	matplot(v_x, m, lty=1, pch=1, xlab="Time", ylab="CDF", type="l")
	return (m)
}


