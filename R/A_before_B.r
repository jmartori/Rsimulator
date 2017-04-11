#!/usr/bin/env Rscript
# Side file that computes the probability that an exponential event happens before another one.
# This is done both using randomly generated numbers and with the formulas.
# Several functions exists to compute only 2,3,4 or n events.

source("./R/utils-base.r")
sim_a_before_b <- function(t, l, iter)
{
	lambda_A <- l
	lambda_B <- l

	times <- t

	values_A <- rexp(iter, lambda_A)
	values_B <- rexp(iter, lambda_B)

	m_AB <- matrix(c(values_A,values_B), ncol=2)

	m_time <- sapply(times, function(t) apply(m_AB, 1, function(x) x[1] - (x[2] + t)))
	v_prob <- sapply(1:length(times), function(t) count(which(m_time[, t]>0)))/iter

	return(v_prob)
}


eq_a_before_b <- function (t, l)
{
	return (0.5 * l * exp(-l * t)) 
}



sim_a_and_b_before_c <- function(t, l, iter)
{
	lambda_A <- l
	lambda_B <- l
	lambda_C <- l

	times <- t

	values_A <- rexp(iter, lambda_A)
	values_B <- rexp(iter, lambda_B)
	values_C <- rexp(iter, lambda_C)

	m_ABC <- matrix(c(values_A,values_B, values_C), ncol=3)

	m_time <- sapply(times, function(t) apply(m_ABC, 1, function(x) x[3] - (x[1] + x[2] + t)))
	v_prob <- sapply(1:length(times), function(t) count(which(m_time[, t]>0)))

	return(v_prob/iter)
}

sim_a_and_b_and_c_before_d <- function(t, l, iter)
{
	lambda_A <- l
	lambda_B <- l
	lambda_C <- l
	lambda_D <- l

	times <- t

	values_A <- rexp(iter, lambda_A)
	values_B <- rexp(iter, lambda_B)
	values_C <- rexp(iter, lambda_C)
	values_D <- rexp(iter, lambda_D)

	m_ABCD <- matrix(c(values_A,values_B, values_C, values_D), ncol=4)

	m_time <- sapply(times, function(t) apply(m_ABCD, 1, function(x) x[4] - (x[1] + x[2] + x[3] + t)))
	m_time2 <- sapply(times, function(t) apply(m_ABCD, 1, function(x) x[4] - (x[1] + t)))
	
	v_prob <- sapply(1:length(times), function(t) count( intersect( which(m_time[, t]<0), which(m_time2[, t]>0)) ))
	#v_prob2 <- sapply(1:length(times), function(t) count())

	return(v_prob/iter)
}

# Generalization of the previous ones.
sim_a_n_levels_before_z <- function(t, l, lvl, iter)
{

	times <- t

	m_AZ <- matrix(rexp(iter*lvl, l), ncol=lvl)

	m_time <- sapply(times, function(t) apply(m_AZ, 1, function(x) x[1] - (sum(x[2:lvl]) + t)))
	v_prob <- sapply(1:length(times), function(t) count(which(m_time[, t]>0)))

	return(v_prob/iter)
}

eq_a_n_levels_before_z <- function(t, l, lvl)
{

	# shaurien d posar les guards que facin falta.
	return((0.5 * l * exp(-l * t)) ^ (lvl-1))
}

eq_a_b_c_before_d<- function(t, l)
{
  
  # shaurien d posar les guards que facin falta.
  return(3*exp(-l*t)/8)
}


exp_max_diff_min <- function (n, iter, range, l=1)
{
	m <- matrix(rexp(iter*n, l), ncol=iter)

	m_max <- apply (m, 2, max)
	
	val <- m_max - m[1,] 
	return ( sim_prob_time_2( range, val))
}



sim_a_before_b_runif <- function(rate=10, lambda=1, time=10)
{
	n <- rate*time
	v <- sort(runif(n, min=0, max=time))
	ve <- rexp(n, rate=lambda)

	vs <- v + ve


	#return (sapply((n/10):n, function(x) count(which(vs[x] < vs[1:x]))))
	return (sapply(1:(90*n/100), function(x) count(which(vs[x] > vs[x:n]))))

}

sim_a_before_b_runif_2 <- function(rate=10, lambda=1, time=10)
{
	n <- rate*time
	v <- sort(runif(n, min=0, max=time))
	v_a <- rexp(n, rate=lambda)
	v_b <- rexp(n, rate=lambda)	
	m <- matrix(c(v_a,v_b,v), ncol=3)
	


	# has de provar lo k estavaem fent abans de tot amb valors k siguin random d veritat i llavors mirem les pussybilitats de k aixo sigui aixi.
	return (apply(m, 2, function(x) x[1] > x[2:3]))
}