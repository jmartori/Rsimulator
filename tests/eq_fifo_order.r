source("./R/utils.r")
source("./tests/utils-test.r")



rcv_eq_fifo_order <- function(k=24, r=10, l=1, t=0, n=5)
{
	f <- function (t, k, r, n)
	{
		#return ((exp(l*t)/n)*(1 - ( 1 - exp(-l*t))^n ))
		return (sapply(0:k, function (i) ( (exp(l*(t + i/r ))/n)*(1 - ( 1 - exp(-l*(t + i/r)))^n )) ))
	}

	v <- sapply(t, function(taux) {
		term <- prod( f(taux, k, r, n))
		return(term)
	})
	return (v)
}


comb_eq_fifo_order <- function(k=24, r=10, l=1, t=0)
{
	f <- function(k, l, r, t){
		prod(sapply(0:k, function (i) (1 - exp(-l*(t + i/r))) ))
	}
	v <- sapply(t, function(taux) {
			p <- f(k, l, r, taux)
			#printf("%f\t%f\t%f\t%f\t%f \n",p, k, l, r, t)
			return( 1 - (1-p)^5) # binomial  (X >= 1) -> k=0
		})

	return (v)
}

eq_fifo_order_ng <- function(n_op, k=24, r=2, l=1, t=0, n=2, iter=10000)
{
  pad <- round(0.1*iter)
  config <- get_scenario(p_rpareto=0, len_lambda=l, rate_op=r, window_op=iter, padding_op=pad, num_repl = n)
  r_data <- process_scenario(config, f_dc=matrix_causal_delivery_time_bt, f_dnc=matrix_delivery_time)
  
  sapply(n_op, function(op){
    repl_op <- apply(r_data$dnc, 1, which.min)
    repl <- repl_op[op]
    
    belong_repl <- which(repl_op == repl)
    before_n_op <- belong_repl[which(belong_repl < op)]
    times <- r_data$dnc[before_n_op,repl]
    t0 <- r_data$dnc[op, repl]
    
    return(p <- sapply(t, function(ti) exp_fifo_ng(t = (t0-times)+ti, l=l)))
  })-> ret_val
  return(apply(ret_val, 1, mean))
}

eq_fifo_order <- function(k=24, r=2, l=1, t=0)
{
	return (sapply(t, function(taux) prod(sapply(0:k, function (i) (1 - exp(-l*(taux + i/r))) ))))
}

sim_fifo_order <- function(n=5, k=24, r=10, l=1, t=0, iter=2500, ... ) # How do we use the iter??
{
	# 
	# We are not currently using the t(p)*r or n variable.
	#
	config <- get_scenario(num_repl=n, len_lambda=l, rate_op=(r*n), window_op=iter, padding_op=round(0.1*iter),  ...)
	data <- process_scenario(config, f_fifo=matrix_fifo_delivery_time, f_dnc=matrix_delivery_time)
	
	# Visibility
	#d_fifo <- get_operation_delivery_time(data$fifo)

	# Latency
	d_fifo <- apply (data$fifo, 1, function(x) x - min(x) )

	return(sim_prob_time(t, d_fifo[d_fifo!=0]))
}

plot_fifo_order <- function(v_times, v, w, m=NULL) 
{
	if (is.null(m)) {
		plot(v_times, get_y_axis(v_times), type="n")
	}

	points(v_times, v, type="l", col="green")
	points(v_times, w, type="l", col="blue")

	m <- rbind (m, v)
	m <- rbind (m, w)

	return (m)
}