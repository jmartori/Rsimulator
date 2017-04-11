
source("./R/utils.r")
source("./tests/utils-test.r")

eq_false_positive <- function(last_k=0, k=0, r=10, l=1, t=0)
{
	# last_k: last received operation
	# l: lambda of the exponential distributions
	# k: is the set of missing operations
	# r: is the rate of operations
	# t: is the waiting time of the system (c1_dt)
	#
	return (1 - prod(sapply (k, function(i) 1 - 3*exp(-l*(t + (last_k - i)/r))/8)))
}

sim_false_positive <- function(k=24, r=10, l=1, t=0, n=5, iter=2500)
{
	# 
	# We are not currently using the t(p)*r or k variable.
	#
	config <- get_scenario(p_rpareto=0, len_lambda=l, rate_op=r, window_op=iter, padding_op=round(0.1*iter), c1_dt=0, c2_dt=10000)
	data <- process_scenario(config, f_dc_er=matrix_causal_delivery_time_bt_with_error_recovery, f_dnc=matrix_delivery_time)
	
	d_dc <- get_operation_delivery_time(data$dc_er$data)
	fp <- data$dc_er$log$fp
	return(sim_prob_time(t, d_dc))
}

plot_false_positive <- function(v_times, v, w, m=NULL) 
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