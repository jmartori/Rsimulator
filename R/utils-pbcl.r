source("./R/utils-base.r")

summary_pbcl <- function(data)
{
	s_data <- sort(data)
	len <- length(s_data)

	printf("Min\t%f\n", s_data[1])
	printf("2\t%.3f\n", s_data[round(0.02*len)])
	printf("10\t%f\n", s_data[round(0.1*len)])
	printf("25\t%f\n", s_data[round(0.25*len)])
	printf("50\t%f\n", s_data[round(0.5*len)])
	printf("75\t%f\n", s_data[round(0.75*len)])
	printf("95\t%f\n", s_data[round(0.95*len)])
	printf("98\t%f\n", s_data[round(0.98*len)])
	printf("99\t%f\n", s_data[round(0.99*len)])
	printf("99.9\t%f\n", s_data[round(0.999*len)])
	printf("Max\t%f\n", s_data[len])
	printf("Mean\t%f\n", mean(s_data))
	printf("sd\t%f\n", sd(s_data))
}


give_dependent_op <- function (data)
{
	f_give_dependent_op <- function(op)
	{
		r <- which.min(data[op, ])
		list <- which(data[,r] <= data[op, r])
		return (list)
	}

	return(sapply(1:length(data[,1]), function(op) f_give_dependent_op(op)))

}

op_in_lambda <- function(data, lambda)
{
	aux_op_in_lambda <- function(x, t, data, repl_op)
	{
		r <- repl_op[x]
		index <- which(repl_op == r)
		index <- index[which(index < x)]
		return ( length(which(data[index, r] >= (data[x,r]-t) )))
	}

	repl_op <- apply(data, 1, which.min)
	median_time <- log(2, base = exp(1)) / lambda


	list <- sapply(1:length(data[,1]), function(x) aux_op_in_lambda(x, median_time, data, repl_op))


	return (list)
}

get_delay <- function(data)
{
	min_op <- apply(data, 2, min)
	repl_op <- apply(data, 1, which.min)
	list <- NULL
	for (op in 1:length(data[ ,1]))
	{
		r <- repl_op[op]
		if (data[op, r] > min_op[r]){
			index <- which(data[ ,r] < data[op, r])
			new_delta <- data[op, r] - max(data[ index, r])
			# as.numeric so we remove the replica label from the matrix
			list <- c(list, as.numeric(new_delta))
		} else
			#printf(">> %d\n", op)
			list <- c(list, 0)
	}
	return (list)
}

f_get_past_op <- function(data, i, lambda){
	r <- which.min(data[i, ])
	min_op <- apply(data, 2, which.min)
	if ( intersect(min_op, i) == i) return (1)

	n <- length(data[1, ])

	repl_op <- apply(data, 1, which.min)
	list <- which(data[,r] < data[i, r])

	op_list <- which(sapply(repl_op, function(x) x==r))
	op_list <- op_list[op_list < i]
	return (prod(sapply(op_list, function(op) (1-exp(-lambda*(data[i,r] - data[op,r])))^(n-1) )))
}
	

prob_each_op <- function(dep_data, data, dnc , lambda, repl_op, t)
{
	f_prob_each_op <- function(deps, n)
	{
		op <- max(deps)
		r <- repl_op[op]
	
		#return (sapply(deps, function(i) if (repl_op[i] == r ) 
		#									return ((1-exp(-lambda * (t + (data[op, r] - data[i, r] ) )))^(n-1) )
		#						 		else return ((1-0.5*exp(-lambda * (t + (data[op, r] - data[i, r] ) )))^(n-2))
		#		))

		# Cheatah's formula
		#return (sapply(deps, function(i) if (repl_op[i] == r ) 
		#									return ((1-exp(-lambda * (t + (data[op, r] - data[i, r] ) )))^(n-1) )
		#						 		else return ((1-exp(-lambda * (t + (data[op, r] - data[i, repl_op[i]] ) )))^(n-2))
		#		))

		return (sapply(deps, function(i) if (repl_op[i] == r ) 
											return ((1-exp(-lambda * (t + (data[op, r] - data[i, r] ) )))^(n-1) )
								 		# The dnc for reception time without causality
								 		else return ((1-0.5*exp(-lambda * (t + (data[op, r] - dnc[i, r] ) )))^(n-2))
				))

	
	}

	n <- length(data[1,])
	return(sapply(dep_data, function(deps)  prod(f_prob_each_op(deps, n))))	
}
#
# 
# Comment [jordi] : I agree it's a bad function name. But ...
# the change from the previous prob_each_op is the remote equation from 1 - 0.5e^-lt to e^lt/n *(1-(1-e^-lt)^n)
# 
# k check the last dependent operations (and not all of them)
prob_each_op_2 <- function(dep_data, data, dnc , lambda, repl_op, t, k=50)
{
  f_prob_each_op <- function(deps, n)
  {
    op <- max(deps)
    r <- repl_op[op]

    return (sapply(deps, function(i) if (repl_op[i] == r ) 
      return ((1-exp(-lambda * (t + (data[op, r] - data[i, r] ) )))^(n-1) )
      # The dnc for reception time without causality
      else return ((exp(lambda*(t + (data[op, r] - dnc[i, r] ) ))/n)*(1 - ( 1 - exp(-lambda * (t + (data[op, r] - dnc[i, r] ) )))^n) )
    ))
    
    
  }
  
  n <- length(data[1,])
  return(sapply(dep_data, function(deps)  prod(f_prob_each_op(max_n(deps, k), n))))	
}

# Yes its becomming a bad habit but...
#
# This one takes into account get_prob_depen_dist from R/utils.r and combines it with remote visibility to compensate for the messages that 
# op doesnt depen on.
prob_each_op_3 <- function(dep_data, data, dnc , lambda, repl_op, t, k=50, p_dist)
{
  f_prob_each_op <- function(deps, n)
  {
    op <- max(deps)
    r <- repl_op[op]
    
    return (sapply(deps, function(i) {
      if (repl_op[i] == r ) {
        return ((1-exp(-lambda * (t + (data[op, r] - data[i, r] ) )))^(n-1) )
      }else{
        # Probability visibility of a remote message
        p_r <- (exp(lambda*(t + (data[op, r] - dnc[i, r] ) ))/n)*(1 - ( 1 - exp(-lambda * (t + (data[op, r] - dnc[i, r] ) )))^n)        
        # Probability that this message depends on op
        p_d <- p_dist[op - i]
        # compensating p_r for p_d with a liniar equation.
        return (1-(1-p_r)*p_d)
      }
      
    }))    
  }
  
  n <- length(data[1,])
  return(sapply(dep_data, function(deps)  prod(f_prob_each_op(max_n(deps, k), n))))  
}

# Doesnt Work Yet. Because we take operation with a negative time...
prob_each_op_4 <- function(dep_data, data, dnc , lambda, repl_op, t, p_dist, k=50)
{
  f_prob_each_op <- function(op, n)
  {
    r <- repl_op[op]
    
    #check better if should be op or op-1
    op_ids <- max_n(op:(op-k), k)
    op_ids <- op_ids[op_ids>0]
    return (sapply(op_ids, function(i){
      
      if (repl_op[i] == r ) {
        return ((1-exp(-lambda * (t + (data[op, r] - data[i, r] ) )))^(n-1) )
      }else{
        # Probability visibility of a remote message
        p_r <- (exp(lambda*(t + (data[op, r] - dnc[i, r] ) ))/n)*(1 - ( 1 - exp(-lambda * (t + (data[op, r] - dnc[i, r] ) )))^n)        
        # Probability that this message depends on op
        p_d <- p_dist[op - i]
        # compensating p_r for p_d with a liniar equation.
        return (1-(1-p_r)*p_d)
      }
    }))
  }
  
  #Number of replicas
  n <- length(data[1,])
  
  # We should change it to just holding the window operations
  #1:dim(data)[1]
  # Currently its manual... and bad...
  return(sapply(1001:11000, function(op)  prod(f_prob_each_op(op, n))))  
}



# The local equation is based on the rate of operation and not delivery time
prob_each_op_rate <- function(dep_data, data, dnc , lambda, rate, repl_op, t)
{
	f_prob_each_op <- function(deps, n)
	{
		op <- max(deps)
		r <- repl_op[op]
	
		#return (sapply(deps, function(i) if (repl_op[i] == r ) 
		#									return ((1-exp(-lambda * (t + (data[op, r] - data[i, r] ) )))^(n-1) )
		#						 		else return ((1-0.5*exp(-lambda * (t + (data[op, r] - data[i, r] ) )))^(n-2))
		#		))

		# Cheatah's formula
		#return (sapply(deps, function(i) if (repl_op[i] == r ) 
		#									return ((1-exp(-lambda * (t + (data[op, r] - data[i, r] ) )))^(n-1) )
		#						 		else return ((1-exp(-lambda * (t + (data[op, r] - data[i, repl_op[i]] ) )))^(n-2))
		#		))

		return (sapply(deps, function(i)
			if (repl_op[i] == r ) 
				return ((1-exp(-lambda * (t  )))^(n-1) )
				# The dnc for reception time without causality
			else return ((1-0.5*exp(-lambda * (t + (data[op, r] - dnc[i, r] ) )))^(n-2))
				))

	
	}

	n <- length(data[1,])
	return(sapply(dep_data, function(deps)  prod(f_prob_each_op(deps, n))))	
}

prob_dnc <- function(data, lambda, t)
{
	n <- length(data[1,])

	return((1-exp(-lambda * t))^(n-1) )
}

prob_with_rate <- function(v_times, lambda, rate_op, prob)
{
	k <- trunc(  (-ln(1-prob) / (rate_op * lambda)) + 1 ) # its to get the round up
	#print(k)
	val <- sapply(v_times, function(t) prod(sapply(1:k, function(x) 1 - exp(-lambda * (t + (x * rate_op))))))
	
	return (val)
}

eq_generic <- function(v_times, l, num_repl, rate, prob)
{
	# S'hi ha de seguir treballant perq encara no funciona b.
	n <-  trunc((-ln(1-prob))/(l * rate)) + 1
 
 	val <- sapply(v_times , function(t) prod(sapply(1:round(n/num_repl), function(i) 1 - exp(-l * (t + (i * rate))) )) * prod(sapply (1:round( n * (num_repl-1)/num_repl), function(i) 1 - 0.5*exp(-l * (t + ( i * rate))))))
	return (val)
}

eq_prob_time <- function(vx_coor, deps, m_data, config, f=prob_each_op_2,...)
{
	return(sapply( vx_coor , function (t) mean(f(deps, m_data$dc, m_data$dnc, config$len_lambda, config$repl_op, t,...))))
}

sim_cond_prob_time <- function(v_times, m_data, config) {
  stop("Not yet finished")
  f_each_op <- function(t, repl_op, config, kr_max) {
    term1 <- 1
    term2 <- 1
  }
  return ( sapply(v_times, function(t) {
    f_each_op(t, m_data, config, kr_max)  
  }))
}

prob_dep_deliv_fixed <- function(v_times=seq(0,15,0.1), lat_dc, repl_op=NULL) {
  if (is.null(repl_op)) {
    # if two replicas have zero, there could be a little problem.
    repl_op <- apply(lat_dc, 1, which.min)
  }
  sapply(1:ncol(lat_dc), function(n) {
    rx_lat_dc <- lat_dc[which(repl_op != n), n]
    p_rx <- sim_prob_time(v_times, rx_lat_dc)
  }) -> m
  rx_mean <- apply(m, 1, mean)
  
  return(cbind(m, rx_mean))
}

get_repl_op <- function(data)
{
  return (apply(data, 1, which.min))
}

sim_deliv_time <- function(m_data, k=50, r=1, n=nrow(m_data$dc))
{
   
  sapply((k+1):n, function(op) {
    return(m_data$dc[op, r] - m_data$dnc[(op-1):(op-k), r])
  }) -> di
  return (di)
}