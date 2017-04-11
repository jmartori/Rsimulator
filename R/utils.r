source("./R/utils-base.r")
source("./R/utils-latmod.r")
source("./R/utils-datagen.r")
suppressMessages(library("actuar"))

	

get_points_by_repl <- function(t_op, repl_op, id_repl)
{
	list_points <- NULL
	for ( i in 1:length(repl_op))
	{
  	if (repl_op[i] == id_repl) {
			list_points <- c(list_points, t_op[i])
		}
	}
	return(list_points)
}

get_id_points_by_repl <- function(t_op, repl_op, id_repl)
{
	list_ids <- NULL
	for ( i in 1:length(repl_op))
	{
		if (repl_op[i] == id_repl) {
			list_ids <- c(list_ids, i)
		}
	}
	return(list_ids)
}
get_depedent_operations <- function(op, t_op, repl_op, dis_op)
{
	issue_time <- t_op[op]
	v_dependent <- NULL

	for ( i in 1:length(t_op))
	{
		if (i == op) next
		times <- dis_op[((op-1) * num_repl+1):(op * num_repl)]
		times[repl_op[op]] <- 0

		if ((issue_time + times[repl_op[i]]) < (t_op[i]) ) {
			v_dependent <- c(v_dependent, i)
		}

	}
	return (v_dependent)
}

matrix_causal_delivery_time <- function (config)
{
	total_op <- config$total_op
	num_repl <- config$num_repl
	t_op <- config$t_op
	repl_op <- config$repl_op
	dis_op <- config$dis_op

	d <- matrix(nrow = total_op, ncol = num_repl)

	for ( op in 1:total_op)
	{
		for (r in 1:num_repl)
		{
			if (r == repl_op[op]) {
				d[op, r] <- t_op[op]
			} else {
				d[op, r] = t_op[op] + dis_op[op, r]
				#d[op, r] = t_op[op] + dis_op[((num_repl * (op-1)) + r)] # Jo diria que es aquesta dis_op[]
				for(op2 in 1:(op-1))
				{
					if (isTRUE((d[op2, repl_op[op]] < t_op[op]) && (d[op2, r] > d[op, r]))) {
						#printf("%d %d %f %f %f %f\n", op, op2, d[op,r],d[op2,r], d[op2, repl_op[op]], t_op[op])
						d[op, r] <- d[op2, r]
					}
					#if (isTRUE( (repl_op[op2] 2== repl_op[op]) && (t_op[op2] < t_op[op]) && (d[op2, r] > d[op, r] ) )  ) {
					#	d[op, r] <- d[op2, r]
					#}
				}
			}
		}
	}

	return(d)
}

matrix_causal_delivery_time_bt <- function (config)
{
	total_op <- config$total_op
	num_repl <- config$num_repl
	t_op <- config$t_op
	repl_op <- config$repl_op
	dis_op <- config$dis_op

	d <- matrix(nrow = total_op, ncol = num_repl)
	check <- vector(length=num_repl)
	
	for ( op in 1:total_op)
	{
		dep_op <- NULL
		check <- rep(0, num_repl)
	
		if (op > 1) {
			for(op2 in ((op-1):1))
			{	
				#printf("%d %d %f %f %d\n", op, op2, d[op2, repl_op[op]], t_op[op], repl_op[op])

				if (d[op2, repl_op[op]] < t_op[op]) {
					#printf("%d %d %f %f %f %f\n", op, op2, d[op,r],d[op2,r], d[op2, repl_op[op]], t_op[op])
					dep_op <- c(dep_op, op2)
					check[repl_op[op2]] <- 1
					if (length(check[check==0])==0) break
				}
			}
		}
		for (r in 1:num_repl)
		{

			if (r == repl_op[op]) {
				d[op, r] <- t_op[op]
			} else {
				if ( is.null(dep_op) ) {
					d[op, r] <- t_op[op] + dis_op[op, r]
				} else {
					d[op, r] <- max(c(t_op[op] + dis_op[op, r], max(d[dep_op, r]))) 
				}
			}
		}
	}
	return(d)
}

matrix_fifo_delivery_time <- function (config)
{
	total_op <- config$total_op
	num_repl <- config$num_repl
	t_op <- config$t_op
	repl_op <- config$repl_op
	dis_op <- config$dis_op

	d <- matrix(nrow = total_op, ncol = num_repl)
	op_aux <- 0
	for ( op in 1:total_op) {
		if (op > 1 ){
			for (op2 in (op-1):1){
					if (repl_op[op2] == repl_op[op] ) {	
						op_aux <- op2	
						break
					}	
			}
		}

		for (r in 1:num_repl) {
			if (r == repl_op[op]) {
				d[op, r] <- t_op[op]
			} else {
				d[op, r] <- t_op[op] + dis_op[op, r]
				if (op_aux > 0) {
					if (d[op, r] < d[op_aux, r]){
						d[op, r] <- d[op_aux, r]
					}
				}
			}
		}
	}

	return(d)
}


matrix_delivery_time <- function (config)
{
	total_op <- config$total_op
	num_repl <- config$num_repl
	t_op <- config$t_op
	repl_op <- config$repl_op
	dis_op <- config$dis_op

	e <- matrix(nrow = total_op, ncol = num_repl)

	for ( op in 1:total_op)
	{
		for (r in 1:num_repl)
		{
			if (r == repl_op[op]) {
				e[op, r] <- t_op[op]
			} else {
				e[op, r] <- t_op[op] + dis_op[op, r]
			}
		}
	}
	
	return(e)
}

operation_events <- function(method=c("unif", "exp", "exp_sbs", "par", "par_sbs", "static", "unif_sbs"), rate_op, num_ops){
  time_exp <- num_ops/rate_op
  if(method == "exp1"){
    v <- sort(rexp(num_ops, rate=(2/time_exp)))
    return (v)
  } 
  if (method == "par"){
    v <- sort(rpareto1(num_ops, shape = 2, min = time_exp/4 ))
    return (v)
  }
  if (method == "static"){
    v <- seq(0,time_exp, time_exp/num_ops)
    # We remove 0.0
    return (v[which(v>0.0)])
  }
  # sbs: step by step
  if (method == "exp_sbs"){
    v <- cumsum(rexp(num_ops, rate = rate_op))
    return (v)
  } 
  if (method == "par_sbs"){
    v <- cumsum(rpareto1(num_ops, min=0.01*rate_op^-1, shape=1.001001001))
    return (v)
  }
  if (method == "unif_sbs"){
    v <- cumsum(runif(num_ops, 0, 2*rate_op^-1))
    return (v)
  }
  if (method == "norm_sbs"){
    v <- rnorm(2*num_ops, mean = rate_op^-1, sd = 5*rate_op^-1)
    v <- cumsum(v)
    v <- sort(v)
    return (v)
  } else {
    # uniform distribution is the default one.
    
    # Generate both, time, operation and reception times
    v <- sort(runif (num_ops, 0, time_exp))
  }
  return (v)
}

# whatever value you add to c2_dt will be overwritten by the compute_rto function
get_scenario <- function (f_tot=NULL, num_repl=5, weight_repl_op = rep.int(1, num_repl), rate_op=10, padding_op=100, window_op=1000, len_lambda=0.1, p_rpareto=0, pareto_shape=0.235, pareto_scale=10, c1_dt=NA, c2_dt=NA, t_op_method = "unif")
{
	# This way we can control the randomness
	#set.seed(seed_value)
	# total_op/time_exp = rate_op
	
	config <- list ( rate_op=rate_op, padding_op=padding_op, window_op=window_op, c1_dt=c1_dt, p_rpareto=p_rpareto, pareto_scale=pareto_scale, pareto_shape=pareto_shape, num_repl=num_repl, len_lambda=len_lambda)


	# 50k is an arbitrary number of op to get a statistically significant value of rto.
	rto <- compute_rto (50000, get_transmission_operation_times, config)

	# Generate both, time, operation and reception times
  t_op <- operation_events(method=t_op_method, rate_op = rate_op, num_ops = (window_op+2*padding_op))
	
	repl_op <- sample (1:num_repl, window_op+2*padding_op, replace = TRUE, prob = weight_repl_op)
	
	v_operations <- get_transmission_operation_times(num_repl*(window_op+2*padding_op), config, f=f_tot)

	dis_op <- matrix (data=v_operations, nrow=(window_op+2*padding_op), ncol=num_repl)

	# config is a list of all parameters
	config <- list ( rate_op=rate_op, padding_op=padding_op, window_op=window_op, t_op=t_op, repl_op=repl_op, dis_op=dis_op, total_op=window_op+2*padding_op, c1_dt=c1_dt, c2_dt=c2_dt, p_rpareto=p_rpareto, pareto_scale=pareto_scale, pareto_shape=pareto_shape, num_repl=num_repl, len_lambda=len_lambda, c2_dt=rto)

	return (config)
}

# Given an scenerio configuration and and the name of the processing functions we get a matrix with the delivery time for each operation to each replica
process_scenario <- function (config, f_dc=NULL, f_dnc=NULL, f_fifo=NULL, f_dc_er=NULL, b_fifo=FALSE, show_time=FALSE) {
	
	m_dc <- NULL
	m_dnc <- NULL
	m_fifo <- NULL
	m_dc_er <- NULL

	ptm0 <- proc.time()[3] 


	if (! is.null(f_dc) ) m_dc  <- f_dc(config)
	
	if (! is.null(f_dnc) ) m_dnc <- f_dnc(config)

	if (! is.null(f_fifo) ) m_fifo <- f_fifo(config)

	if (! is.null(f_dc_er) ) m_dc_er <- f_dc_er(config, b_fifo=b_fifo)


	# We create a list with all output values.
	m_data <- list("dc"=m_dc, "dnc"=m_dnc, "fifo"=m_fifo, "dc_er"=m_dc_er)
	

	if (show_time) printf("%f\n", proc.time()[3] - ptm0)
	return (m_data)
}


compute_rto <- function(num_op=50000, f, ...){
	v <- f(num_op, ...)

	v <- 2*v
	mean_v <- mean(v)
	k_mad <- mean(abs(mean_v - v))
	rto <- mean_v + 4*k_mad
	return (rto)
}

#
# Depracated and split between get sceneario and process scenario
#
#causal_simulation <- function (num_repl=5, time_exp=100, rate_op=10, len_lambda=0.1, seed=42, plot_me_please=FALSE, p_rpareto=0.08, pareto_shape=0.235, pareto_scale=10, f_dc=NULL, f_dnc=NULL, f_fifo=NULL, f_dc_er=NULL, p_times=FALSE, l_dt=24, c1_dt=100, c2_dt=124) {
#	#set.seed(seed)
#	
#	total_op <- time_exp * rate_op
#
#	# latency of 0.7 per round is 12, with two rounds its 0.7*0.7 ~ 0.5 and its 24
#	config <- list ( total_op=time_exp*rate_op, l_dt=l_dt, c1_dt=c1_dt, c2_dt=c2_dt, p_rpareto=p_rpareto, pareto_scale=pareto_scale, pareto_shape=pareto_shape, num_repl=num_repl, len_lambda=len_lambda)
#
#	p_rexp <- 1 - p_rpareto
#
#	# First we need to generate the operations
#	if (p_times) ptm0 <- proc.time()[3]
#	# We create all the TS operations
#	t_op <- sort(runif (total_op, 0, time_exp))
#	#t_op <- sort(rpareto(total_op, 1, 1))
#
#	if (p_times) ptm1 <- proc.time()[3]
#	# Link operation to the replicas
#	repl_op <- sample (1:num_repl, total_op, replace = TRUE)
#	
#	if (p_times) ptm2 <- proc.time()[3]
#	# Distribution of the operation
#	
#
#	# With pareto
#	dis_op <- matrix(get_transmission_operation_times(config$num_repl * config$total_op, config) , nrow = config$total_op, ncol = config$num_repl )
#	#dis_op <- matrix(sample(c( rpareto(round(p_rpareto * num_repl * total_op), pareto_shape, pareto_scale ), rexp (round(p_rexp * num_repl * total_op), rate = len_lambda))), nrow = total_op, ncol = num_repl )
#
#	if (p_times) ptm3 <- proc.time()[3]
#	# We reproduce it to see what happened.
#	# For each operation we compute the causal delivery time.
#
#	m_dc <- NULL
#	m_dnc <- NULL
#	m_fifo <- NULL
#	m_dc_er <- NULL
#
#
#	if (! is.null(f_dc) ) m_dc  <- f_dc(total_op, num_repl, t_op, repl_op, dis_op)
#	if (p_times) ptm4 <- proc.time()[3]
#
#	if (! is.null(f_dnc) ) m_dnc <- f_dnc(total_op, num_repl, t_op, repl_op, dis_op)
#
#	if (p_times) ptm5 <- proc.time()[3]
#
#	if (! is.null(f_fifo) ) m_fifo <- f_fifo(total_op, num_repl, t_op, repl_op, dis_op)
#
#	if (p_times) ptm6 <- proc.time()[3]
#
#	if (! is.null(f_dc_er) ) m_dc_er <- f_dc_er(total_op, num_repl, t_op, repl_op, dis_op, config)
#
#	if (p_times) ptm7 <- proc.time()[3]
#
#	if (plot_me_please)
#	{
#		plot_filename <- sprintf('./plot/pbs-wocd-%d.svg', gk_session_id)
#		plotting(m_dnc, plot_filename, repl_op, t_op)
#
#		plot_filename <- sprintf('./plot/pbs-wcd-%d.svg', gk_session_id)
#		plotting_causality(m_dc, plot_filename, repl_op, t_op)
#	}
#
#
#	# Aixo potser hauria d'estar a fora de la funcio, pero sino no funciona.
#	#casually_torturing_the_data(m_dc,m_dnc)
#
#
#	# v_dc <- vector_prob_repl_time(m_dc)
#	#m_data <- c(m_dc)
#	#m_data <- c(m_data, m_dnc)
#	#m_data <- c(m_data, m_fifo)
#
#	m_data <- list("dc"=m_dc, "dnc"=m_dnc, "fifo"=m_fifo, "dc_er"=m_dc_er)
#
#	#print(m_dc)
#	#print(m_dnc)
#
#	if (p_times) printf("t_dc_er = %.5f\nt_fifo = %.5f\nt_dnc = %.5f\nt_dc = %.5f\nt_dis_op = %.5f\nt_repl_op = %.5f\nt_runif = %.5f\n", ptm7 - ptm6, ptm6 - ptm5, ptm5 - ptm4, ptm4 - ptm3, ptm3 - ptm2, ptm2 - ptm1, ptm1 - ptm0)
#
#	return (m_data)
#}

# Funcio auxiliar per un sapply

sim_prob_time <- function(v_times, data) {
	f <- function(data, t) return(length(data[data<t]) / length(data))

	return (sapply(v_times, function(x) f(data, x)))	
}

l_completed_transmissions <- function(data, x) {
	m <- apply(data, 1, max)
	
	return(length(m[m<x]) / length(m))
}


f_error_recovery <- function(time_op, time_op_dep, original_time, config)
{
	c1_detect_time <- config$c1_dt
	c2_detect_time <- config$c2_dt
	
	new_dist_time <- time_op_dep
	fp <- 0
	retries <- 0
	gain <- 0

	done <- FALSE
  change <- FALSE
	#printf("Begin *****\n")
 	#printf("%f \n", new_dist_time)

	while (done == FALSE) {
		# t has to be stored 
		t <- sum(get_transmission_operation_times(2, config))

		new_dist_time <- min (new_dist_time, time_op + c1_detect_time + (retries*c2_detect_time) + t)
	#	printf("%f \n", new_dist_time)
		if (new_dist_time < time_op + c1_detect_time + ((retries+1)*c2_detect_time) ){
			done <- TRUE

			if (new_dist_time  == original_time ) {
				fp <- 1
				gain <- 0
        		change <- time_op_dep != original_time
			} else {
				gain <- time_op_dep - new_dist_time
       		 	change <- TRUE
			}
	#		printf("*********\n")
		}
		retries <- retries + 1

	}

	return( list(d=new_dist_time, fp=fp, retries=retries, gain=gain, change=change))
}


missing_dep_op <- function(op, r, dep_op, dis_op, config)
{
	#browser()
	e <- new.env()

	e$checked <- rep(FALSE, op-1)
	f_dnc_missing <- function (op)
	{
		return (config$t_op[op] + dis_op[op, r])
	}

	f_aux <- function (aux_op, e) {
		missing_ops <- NULL
		for ( op2 in dep_op[[aux_op]]){
			vif <- ( f_dnc_missing(op)  < f_dnc_missing(op2) ) && !e$checked[op2]
			if ( vif ) {
				e$checked[op2] <- TRUE
				missing_ops <- c(missing_ops, op2, f_aux(op2, e) )
			}
		}
		return(missing_ops)
	}

	# First Call of recursive function.
	return(f_aux(op, e))
}


has_intersection <- function (s, t)
{
  return (length(intersect(s, t)) > 0) 
}


matrix_causal_delivery_time_bt_with_error_recovery <- function (config, b_fifo=FALSE)
{


	f_dnc <- function (op, r, dis_op)
	{
		return (config$t_op[op] + dis_op[op, r])
	}

	# Recode me in a functional way, please
	# or just avoid ifs and fors.
	f_fifo_dependent_operations <- function(op, d) {
		if (op > 1) {
			for(op2 in ((op-1):1))
			{	
				if (config$repl_op[op] == config$repl_op[op2]) return(op2)
			}
		}
		return(NULL)
	}
	f_causal_dependent_operations <- function (op, d) {
		dep_op <- NULL
		check <- rep(0, config$num_repl)
	
		if (op > 1) {
			for(op2 in ((op-1):1))
			{	
				vif <- d[op2, config$repl_op[op]] < config$t_op[op]
				if ( vif ) {
					dep_op <- c(dep_op, op2)
					check[config$repl_op[op2]] <- 1
					if (length(check[check==0])==0) break
				}
			}
		}
		return (dep_op)
	}
  
	f_dc <- function (op, dep_op, r, d, dis_op) {
    if (is.null(dep_op[[op]])) {
      return (config$t_op[op] + dis_op[op, r])
    } else {
		  return (max(c(config$t_op[op] + dis_op[op, r], max(d[dep_op[[op]], r]))))
	  }
	}

	f_dependency <- if (b_fifo) f_fifo_dependent_operations else f_causal_dependent_operations 


	num_repl <- config$num_repl
	total_op <- config$total_op
	t_op <- config$t_op
	dis_op <- config$dis_op

	repl_op <- config$repl_op


	d <- matrix(nrow = total_op, ncol = num_repl)
	check <- vector(length=num_repl)
	
	gain    <- rep.int(0, total_op*num_repl)
	fp      <- rep.int(0, total_op*num_repl)
	retries <- rep.int(0, total_op*num_repl)
	first <- rep.int(0, total_op*num_repl)
	improve_time <- rep.int(0, total_op*num_repl)
	miss_op_count <- rep.int(0, total_op)
  
	dep_op_inv <- NULL
	dep_op <- NULL
  
	for ( op in 1:total_op)
	{	
    dep_op_inv[op] <- list(NULL)
	#printf("%d\n", op)
	dep_op[op] <- list(f_dependency(op, d))
    for (op_dep in dep_op[[op]]) {
      dep_op_inv[[op_dep]] <- c(dep_op_inv[[op_dep]], op) 
    }
		for (r in 1:num_repl)
		{
    		delivery_time <- f_dnc(op, r, dis_op) 
			if (r == repl_op[op]) {
				d[op, r] <- t_op[op]
			} else {
				if ( is.null(dep_op) ) {
					d[op, r] <- delivery_time
        		} else {
					d[op, r] <- f_dc(op, dep_op, r, d, dis_op)
				}
        		changed_op <- NULL
        		v_missing_dep_op <- missing_dep_op(op, r, dep_op, dis_op, config)
        		miss_op_count[op] <- miss_op_count[op] + length(v_missing_dep_op)

				for (op_dep in v_missing_dep_op)
				{
				  index <- (r-1)*config$total_op + op_dep
				  vif <- (delivery_time + config$c1_dt) < f_dnc(op_dep, r, dis_op) && ((first[index] > delivery_time) || (first [index] == 0))
				  if ( vif ) {
												
						r_val <- f_error_recovery (delivery_time, f_dnc(op_dep, r, dis_op), t_op[op_dep] + config$dis_op[op_dep, r], config)

						if (r_val$change) { 
              				if (r_val$d > t_op[op_dep] + config$dis_op[op_dep, r]) browser()
				              dis_op[op_dep, r] <- r_val$d - t_op[op_dep] 
				              changed_op <- c(changed_op, op_dep)
						  # Do a sapply
						}
            			first[index] <- delivery_time
            		
						retries[index] <- r_val$retries
						fp[index] <- r_val$fp

						gain[index] <- r_val$gain	
					}
				}
        
        # update delivery times
				if (!is.null(changed_op)) {          
          			changed_op <- sort(changed_op)
          			while (length(changed_op) > 0) {
            			r_op <- changed_op[1]
            			changed_op <- tail(changed_op, -1)
            			t <- f_dc(r_op, dep_op, r, d, dis_op) 
            			vif <- d[r_op, r] > t
            			if ( vif ) {
              				d[r_op, r] <- t
              				changed_op <- sort(union(changed_op, dep_op_inv[[r_op]]))
            			}	
            			improve_time[index] <- improve_time[index] + 1
          			}
        		}
			}
		}
	}
	l <- list(gain=gain, fp=fp, retries=retries, first=first, improve_time=improve_time, miss_op_count=miss_op_count)
	
	return( list( "data"=d, "log"=l) )
}




# Given an op and dnc, it gives which future operations depen on this one.
find_next_op <- function(op, m_dnc)
{
	find_next_op_r <- function(op_by_repl, r, op) 
	{
		aux <- op_by_repl[[r]]
		val <- aux[which(aux > op)[1]]
		return (val)
	}

	num_repl <- length(m_dnc[1,])
	repl_op <- apply(m_dnc, 1, which.min)
	op_by_repl <- sapply(1:num_repl, function(x) which(repl_op == x))
	return(sapply(1:num_repl, function (r) find_next_op_r(op_by_repl, r, op)))
}

print_config <- function (config)
{
	printf("value padding_op = %d\n", config$padding_op)
	printf("count t_op = %d\n", count(config$t_op))
	printf("count repl_op = %d\n", count(config$repl_op))
	printf("count dis_op = %d\n", count(config$dis_op))
	printf("value total_op = %d\n", config$total_op)
	printf("value c1_dt = %d\n", config$c1_dt)
	printf("value num_repl = %d\n", config$num_repl)
	printf("value len_lambda = %.2f\n", config$len_lambda)
	printf("value time_exp = %d\n", config$time_exp)
	printf("value rate_op = %f\n", config$window_op / config$time_exp)
}

sim_prob_time_2 <- function(v_times, data) {
	f <- function(data, t) return(length(data[data<=t]) / length(data))

	return (sapply(v_times, function(x) f(data, x)))	
}


get_operation_delivery_time <- function(m_data) {

	return(apply(m_data, 1, function(row) max(row - min(row))))
}

get_operation_latency_time <- function(data) {
	# data is expected to be a matrix of times per operation
	return (t(apply(data, 1, function(x) x - min(x))))
}

# This is the Parcial quorum version
# Where n is the amount of operations we wait for visibility
# if n=num_repl then it is equal to max.
# return a vector length operations
get_operation_n_latency_time <- function(data, n=nrow(data) ) {
	# data is expected to be a matrix of times per operation
	return(apply(data, 1, function(x) sort(x)[n] - min(x)))
}

get_odt_max_n <- function(m_data,n=1) {
  return(apply(m_data, 1, function(row) max_n(row - min(row),n)))
}

#
# Given the deps from give_dependent_op 
# return a vector length n with the probability that an
# operation depends on another operation distance pos within n
# The probability is computed as the mean o n_max - n_min cases
# of a element in that position being dependent on op or not.
get_prob_depen_dist <- function(deps, n=1000, n_min=n, n_max=length(deps))
{  
  sapply(n_min:n_max, function(op) {
    rn <- rev(op - deps[[op]])
    sd <- setdiff(1:n, rn)
    return (1:n %in% sd)
  }) -> m 
    
  return (1-apply(m, 1, mean))
}

# Idem as the get_prob_depen_dist, but ignores de dependecies from operations from r
get_prob_depen_dist_remote_op <- function(deps, n=1000, repl_op, r=1)
{  
  not_from_r <- which(repl_op != r)
  from_r <- which(repl_op == r)
  
  vp_dist <- NULL
  
  for (d in 1:n) {
    c_r_op <- 0
    c_d_op <- 0
    for (l_op in from_r) {
      op2 <- (l_op-d)
      if (op2 %in% deps[[l_op]]){
        if (op2 %in% from_r){
          # it doesnt matter
        } else {
          c_r_op <- c_r_op + 1
          c_d_op <- c_d_op + 1
        }
      } else {
        if (op2 %in% from_r){
          # it cannot be 
        } else {
          c_r_op <- c_r_op + 1
        }        
      }
    }
    if (c_r_op == 0) {
      vp_dist[d] <- 0
    } else {
      vp_dist[d] <-c_d_op / c_r_op
    }
  }
  
  return(vp_dist)
}

get_prob_deliv_remote_op <- function(dc, dnc, num_repl=5, v_times=seq(0,15,0.1), repl_op=get_repl_op(dnc), ret_m=FALSE) 
{
  vp_dist <- numeric(length(v_times))
  
  cbind(sapply(1:num_repl, function(raux){
    not_from_r <- which( repl_op != raux)
    len_not_from_r <- length(not_from_r)
    
    sapply(v_times, function(t){
      # or change lenght which to sum 
      val <- length(which((dc[not_from_r,raux] - dnc[not_from_r,raux]) <= t ))
      return( val / len_not_from_r)
      
    }) -> vp_dist
    return (vp_dist)
  })) -> m
  if (ret_m)
    return (m)
  else
    return (apply(m,1,mean))
}

#idem as get_prob_deliv_remote_op but easier to explain
get_prob_deliv_remote_op_2 <- function(dc, dnc, num_repl=5, v_times=seq(0,15,0.1)) 
{
  sapply(v_times, function(t) (length(which((dc - dnc) <= t )) - nrow(dc))/((num_repl-1)*nrow(dc)) ) -> m
  return (m)
}



prob_diff_rates_w_pdist <- function( v_rates=c(1,10,100))
{
  probs <- NULL
  v_times <- seq(0, 15, 0.1)
  
  n <- 100
  l <- 1
  
  for (rate in v_rates)
  {
    ptm1 <- proc.time()[3]
    dge <- new.env()
    
    lw <- sim_causal_order(t=v_times, bool=F, dg_env=dge, l=l, r=rate )
    
    vp_dist_deliv <- get_prob_deliv_remote_op(dc = dge$data$dc, dnc=dge$data$dnc, num_repl=5, v_times = seq(0, n, 1/rate))
    vw_l_deliv <- eq_causal_order_w_dist(t=v_times, vp_dist=vp_dist_deliv, l=l, kl_max=n/4, r=rate)
    
    probs <- cbind(probs, lw[[1]], vw_l_deliv)
    
    ptm2 <- proc.time()[3] - ptm1
    print(as.numeric(ptm2))
    
    rm(dge)
  }
  
  return (probs)
}


prob_diff_lambdas_w_pdist <- function( v_lambdas=c(0.05,0.1,0.2,0.5,1), v_times = seq(0, 150, 1), rate=10, n=100)
{
  probs <- NULL
    
  for (l in v_lambdas)
  {
    ptm1 <- proc.time()[3]
    dge <- new.env()
    
    lw <- sim_causal_order(t=v_times, bool=F, dg_env=dge, l=l, r=rate )
    
    vp_dist_deliv <- get_prob_deliv_remote_op(dc = dge$data$dc, dnc=dge$data$dnc, num_repl=5, v_times = seq(0, n, 1/rate))
    vw_l_deliv <- eq_causal_order_w_dist(t=v_times, vp_dist=vp_dist_deliv, l=l, kl_max=n/4, r=rate)
    
    probs <- cbind(probs, lw[[1]], vw_l_deliv)
    
    ptm2 <- proc.time()[3] - ptm1
    print(as.numeric(ptm2))
    
    rm(dge)
  }
  
  return (probs)
}