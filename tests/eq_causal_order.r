source("./tests/utils-test.r")

source("./R/utils-base.r")
source("./R/utils-pbcl.r")
source("./R/utils.r")
source("./R/utils-latmod.r")

#' Equation to compute the probability of being causal with multiple replicas
#' 
#' Computes the probability that all the operations from a replica have been causaly delivered across all the n-replicas.
#'
#' @author "Jordi Martori"
#' @param k's Number of operations that we go back to check. They are the k-most significant operations.
#'          kl_min, kl_max, kr_min, kr_max
#' @param r Rate of Operations per Replica.
#' @param l Lambda for the Exponential distribution.
#' @param t Vector of times that we want to check the probability of delivery. 
#' @param dg_env environment from new.env() that if set will save the state variables from the function there.
#' 
#' @examples 
#' eq_causal_order()
#' 
#' 

# Causal Order Equation Non Generalized  
eq_causal_order_ng <- function(n_op, r=10, l=1, t=0, n=5, kl_min=0, kl_max=24, kr_min=1, kr_max=(kl_max*(n-1)), dg_env=NULL, iter=10000)
{
  pad <- round(0.1*iter)
  config <- get_scenario(p_rpareto=0, len_lambda=l, rate_op=(r*n), window_op=iter, padding_op=pad, num_repl = n)
  r_data <- process_scenario(config, f_dc=matrix_causal_delivery_time_bt, f_dnc=matrix_delivery_time)
  
  deps <- give_dependent_op(r_data$dc)
  repl_op <- apply(r_data$dnc, 1, which.min)
  
  sapply(n_op, function(op){  
    #times <- r_data$dnc[deps[[op]], repl_op[op]]
    t0 <- r_data$dnc[op, repl_op[op]]
    
    sapply(t, function(ti) {
      prod(sapply(deps[[op]], function(op2) {
        v <- ifelse(repl_op[op2] == repl_op[op], exp_snd(t = ti + (t0 - r_data$dnc[op2, repl_op[op]]), l = l), 
                                                 exp_rcv(t = ti + (t0 - r_data$dnc[op2, repl_op[op]]), l = l)
                    )
      }))
    }) -> p
    
    return(p)
  })-> ret_val
  return(apply(ret_val, 1, mean))
}

eq_causal_order_ng_vis <- function(n_op, r=10, l=1, t=0, n=5, kl_min=0, kl_max=24, kr_min=1, kr_max=(kl_max*(n-1)), dg_env=NULL, iter=10000)
{
  pad <- round(0.1*iter)
  config <- get_scenario(p_rpareto=0, len_lambda=l, rate_op=(r*n), window_op=iter, padding_op=pad, num_repl = n)
  r_data <- process_scenario(config, f_dc=matrix_causal_delivery_time_bt, f_dnc=matrix_delivery_time)
  
  deps <- give_dependent_op(r_data$dc)
  repl_op <- apply(r_data$dnc, 1, which.min)
  
  sapply(n_op, function(op){  
    #times <- r_data$dnc[deps[[op]], repl_op[op]]
    t0 <- r_data$dnc[op, repl_op[op]]
    
    sapply(t, function(ti) {
      prod(sapply(deps[[op]], function(op2) {
        v <- ifelse(repl_op[op2] == repl_op[op], 
                    exp_snd_vis(n = n, t = ti + (t0 - r_data$dnc[op2, repl_op[op]]), l = l), 
                    exp_rcv_vis(n = n, t = ti + (t0 - r_data$dnc[op2, repl_op[op]]), l = l)
        )
      }))
    }) -> p
    
    return(p)
  })-> ret_val
  return(apply(ret_val, 1, mean))
}

eq_causal_order <- function(r=10, l=1, t=0, n=5, kl_min=0, kl_max=24, kr_min=1, kr_max=(kl_max*(n-1)), dg_env=NULL)
{
  f_local <- function (t, r)
  {
    return (monotonize(sapply(kl_min:kl_max, function (i) exp_snd_vis(t + i/r,n,l) )))
  }
	
  f_remote <- function (t, r, n, vp_dist)
  {
    return (monotonize(sapply(kr_min:kr_max, function (i) return (exp_rcv_vis(t + i/r, n, l)) )))
  }
  
	if (is.null(dg_env) == FALSE) dg_env$data <- list(length(t))
  
  v <- sapply(1:length(t), function(i) {
    v1 <- f_local(t[i], r)
    v2 <- f_remote(t[i], r*(n-1), n)
    term1 <- prod( v1 )
    term2 <- prod( v2 )
    prod12 <- prod(term1, term2)
    
    # Modification to give support to debug_environment
    if (is.null(dg_env) == FALSE){
      dg_env$data[[i]] <- list ()
      dg_env$data[[i]][["v1"]] <- v1
      dg_env$data[[i]][["v2"]] <- v2
      dg_env$data[[i]][["term1"]] <- term1
      dg_env$data[[i]][["term2"]] <- term2
      dg_env$data[[i]][["prod12"]] <- prod12
    }
    return( prod12 )
  })
   
	return (v)
}

eq_causal_order_w_dist_nvis <- function(vp_dist, r=10, l=1, t=0, n=5, kl_min=0, kl_max=24, kr_min=1, kr_max=(kl_max*(n-1)), dg_env=NULL)
{
  f_local <- function (t, r)
  {
    return (monotonize(sapply(kl_min:kl_max, function (i) exp_snd(t + i/r,l) )))
  }
  
  f_remote <- function (t, r, n, vp_dist)
  {
    return (monotonize(sapply(kr_min:kr_max, function (i){
      p_r <- exp_rcv(t + i/r, l)
      p_d <- vp_dist[i]
      
      return(1 - (1 - p_r)*p_d)
    })))
  }
  
  if (is.null(dg_env) == FALSE) dg_env$data <- list(length(t))
  
  v <- sapply(1:length(t), function(i) {
    v1 <- f_local(t[i], r)
    v2 <- f_remote(t[i], r*(n-1), n, vp_dist)
    term1 <- prod( v1 )
    term2 <- prod( v2 )
    prod12 <- prod(term1, term2)
    
    # Modification to give support to debug_environment
    if (is.null(dg_env) == FALSE){
      dg_env$data[[i]] <- list ()
      dg_env$data[[i]][["v1"]] <- v1
      dg_env$data[[i]][["v2"]] <- v2
      dg_env$data[[i]][["term1"]] <- term1
      dg_env$data[[i]][["term2"]] <- term2
      dg_env$data[[i]][["prod12"]] <- prod12
    }
    return( prod12 )
  })
  
  return (v)
}

# r is the local number of operations
eq_causal_order_w_dist <- function(vp_dist, r=10, l=1, t=0, n=5, kl_min=0, kl_max=24, kr_min=1, kr_max=(kl_max*(n-1)), dg_env=NULL)
{
  f_local <- function (t, r)
  {
    return (monotonize(sapply(kl_min:kl_max, function (i) exp_snd_vis(t + i/r,n,l) )))
  }
  
  f_remote <- function (t, r, n, vp_dist)
  {
    return (monotonize(sapply(kr_min:kr_max, function (i){
         p_r <- exp_rcv_vis(t + i/r, n, l)
         p_d <- vp_dist[i]
         #browser()
         r <- 1 - (1 - p_r)*p_d
         #print(r)
         return(r)
    })))
  }
  
  if (is.null(dg_env) == FALSE) dg_env$data <- list(length(t))
  
  v <- sapply(1:length(t), function(i) {
    v1 <- f_local(t[i], r)
    v2 <- f_remote(t[i], r*(n-1), n, vp_dist)
    term1 <- prod( v1 )
    term2 <- prod( v2 )
    prod12 <- prod(term1, term2)
    
    # Modification to give support to debug_environment
    if (is.null(dg_env) == FALSE){
      dg_env$data[[i]] <- list ()
      dg_env$data[[i]][["v1"]] <- v1
      dg_env$data[[i]][["v2"]] <- v2
      dg_env$data[[i]][["term1"]] <- term1
      dg_env$data[[i]][["term2"]] <- term2
      dg_env$data[[i]][["prod12"]] <- prod12
    }
    return( prod12 )
  })
  
  return (v)
}

eq_causal_order_w_dist_sqrt <- function(vp_dist, r=10, l=1, t=0, n=5, kl_min=0, kl_max=24, kr_min=1, kr_max=(kl_max*(n-1)), dg_env=NULL, nth=2)
{
  f_local <- function (t, r)
  {
    return (monotonize(sapply(kl_min:kl_max, function (i) exp_snd_vis(t + i/r,n,l) )))
  }
  
  f_remote <- function (t, r, n, vp_dist)
  {
    return (monotonize(sapply(kr_min:kr_max, function (i){
      p_r <- exp_rcv_vis(t + i/r, n, l)
      p_d <- vp_dist[i]
      return(1 - (1 - p_r)*nthroot(p_d, nth))
    })))
  }
  
  if (is.null(dg_env) == FALSE) dg_env$data <- list(length(t))
  
  v <- sapply(1:length(t), function(i) {
    v1 <- f_local(t[i], r)
    v2 <- f_remote(t[i], r*(n-1), n, vp_dist)
    term1 <- prod( v1 )
    term2 <- prod( v2 )
    prod12 <- prod(term1, term2)
    
    # Modification to give support to debug_environment
    if (is.null(dg_env) == FALSE){
      dg_env$data[[i]] <- list ()
      dg_env$data[[i]][["v1"]] <- v1
      dg_env$data[[i]][["v2"]] <- v2
      dg_env$data[[i]][["term1"]] <- term1
      dg_env$data[[i]][["term2"]] <- term2
      dg_env$data[[i]][["prod12"]] <- prod12
    }
    return( prod12 )
  })
  
  return (v)
}

eq_causal_order_w_dist_sq <- function(vp_dist, r=10, l=1, t=0, n=5, kl_min=0, kl_max=24, kr_min=1, kr_max=(kl_max*(n-1)), dg_env=NULL, sqn=2)
{
  f_local <- function (t, r)
  {
    return (monotonize(sapply(kl_min:kl_max, function (i) exp_snd_vis(t + i/r,n,l) )))
  }
  
  f_remote <- function (t, r, n, vp_dist)
  {
    return (monotonize(sapply(kr_min:kr_max, function (i){
      p_r <- exp_rcv_vis(t + i/r, n, l)
      p_d <- vp_dist[i]
      return(1 - (1 - p_r)*(p_d^sqn))
    })))
  }
  
  if (is.null(dg_env) == FALSE) dg_env$data <- list(length(t))
  
  v <- sapply(1:length(t), function(i) {
    v1 <- f_local(t[i], r)
    v2 <- f_remote(t[i], r*(n-1), n, vp_dist)
    term1 <- prod( v1 )
    term2 <- prod( v2 )
    prod12 <- prod(term1, term2)
    
    # Modification to give support to debug_environment
    if (is.null(dg_env) == FALSE){
      dg_env$data[[i]] <- list ()
      dg_env$data[[i]][["v1"]] <- v1
      dg_env$data[[i]][["v2"]] <- v2
      dg_env$data[[i]][["term1"]] <- term1
      dg_env$data[[i]][["term2"]] <- term2
      dg_env$data[[i]][["prod12"]] <- prod12
    }
    return( prod12 )
  })
  
  return (v)
}

sim_causal_order <- function(k=24, l=1, t=0, n=5, r=10, iter=10000, dg_env=NULL, bool=TRUE)
{
	# 
	# We are not currently using the t(p)*r or k variable.
	#
  #ptm1 <- proc.time()[3]
  
  pad <- round(0.1*iter)
	config <- get_scenario(p_rpareto=0, len_lambda=l, rate_op=(r*n), window_op=iter, padding_op=pad, num_repl = n)
	data <- process_scenario(config, f_dc=matrix_causal_delivery_time_bt, f_dnc=matrix_delivery_time)
	
  if (bool) {
  # We should only be taking the window operations
    deps <- give_dependent_op(data$dc)
    sim_eq_prob <- eq_prob_time(t, deps, data, config, f=prob_each_op_2)
    vp_dist <- get_prob_depen_dist(deps = deps, n = 3000)
    sim_eq_prob_2 <- eq_prob_time(t, deps, data, config, f=prob_each_op_3, p_dist=vp_dist)
  } else {
    sim_eq_prob <- NULL
    sim_eq_prob_2 <- NULL
    deps <- NULL
    vp_dist <- NULL
  }
  
  # (pad+1):(pad+iter) This way we only take the window operations
	d_dc <- get_operation_delivery_time(data$dc[(pad+1):(pad+iter),])
	sim_prob <- sim_prob_time(t, d_dc)
  
  if (is.null(dg_env) == FALSE) {
    # Store it
    dg_env$config <- config
    dg_env$data <- data
    dg_env$sim_eq_prob <- sim_eq_prob
    dg_env$sim_eq_prob_2 <- sim_eq_prob_2
    dg_env$d_dc <- d_dc
    dg_env$sim_prob <- sim_prob
    dg_env$deps <- deps
    dg_env$vp_dist <- vp_dist
  }
  #print(proc.time()[3] - ptm1)
  return(list(sim_prob, sim_eq_prob, sim_eq_prob_2))
}

plot_causal_order <- function(v_times, v, w, m=NULL) 
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



# vp_dist is against the idea of nb_* functions... 
nb_causal_order <- function (doPlot=TRUE, dg_env = NULL, v_times = seq(0, 15, 0.1), ow_dg_env= FALSE, pdf_name = NULL, vp_dist=NULL, ...) 
{
  #its an empty dg_env and create the lower levels of dg_env 
  if (is.null(dg_env) == FALSE && length(dg_env) == 0){
    dg_env_v <- new.env()
    dg_env_w <- new.env()
    dg_env_vw <- new.env()
  }
  if (is.null(dg_env) == TRUE){
    dg_env_v <- NULL
    dg_env_w <- NULL
    dg_env_vw <- NULL
  }
  if (ow_dg_env == TRUE) {
    # This deletes all the elements in dg_env without changing the reference of dg_env
    rm(list=ls(dg_env), envir=dg_env)
    dg_env_v <- new.env()
    dg_env_w <- new.env()
    dg_env_vw <- new.env()
  }
  if (is.null(dg_env) == TRUE || is.null(dg_env) == FALSE && length(dg_env) == 0 ){
    
    # if its null we have to create it
    if (is.null(vp_dist)){
      dg <- new.env()
      aux_lw <- sim_causal_order(t=v_times, dg_env=dg, bool = F, ...)
      deps <- give_dependent_op(dg$data$dc)
      # n should be acording to t(p)r to save some computing time.
      vp_dist <- get_prob_depen_dist_remote_op(deps = deps, n = 3000, repl_op = get_repl_op(dg$data$dc))
      
      rm(deps, dg, aux_lw)
    }
    print("eq_causal_order_w_dist")
    vw <- eq_causal_order_w_dist(vp_dist, t=v_times, dg_env=dg_env_vw, ...)
    print("eq_causal_order")
    v <- eq_causal_order(t=v_times, dg_env=dg_env_v, ...)
    print("sim_causal_order")
    lw <- sim_causal_order(t=v_times, dg_env=dg_env_w, bool=T, ...)
    
    w <- lw[[1]]
    w_eq <- lw[[2]]
    w_weq <- lw[[3]]
    #vc <- shift(v, rot=(-1*ceiling(mean(closest(v,w,range=seq(0,1,0.01))))))
    
    # p_eq <- eq_prob_time(v_times, give_dependent_op(m_data$dc), m_data, config)
    if (is.null(dg_env) == FALSE) {
      dg_env$v_times <- v_times
      dg_env$v <- v
      dg_env$w <- lw[[1]]
      dg_env$vw <- vw
      #dg_env$vc <- vc
      dg_env$w_eq <- lw[[2]]
      dg_env$w_weq <- lw[[3]]
      dg_env$dg_env_v <- dg_env_v 
      dg_env$dg_env_vw <- dg_env_vw
      dg_env$dg_env_w <- dg_env_w
      
    }
  } else {
    # We load the variables from the dg_env
    # We dont check if its ok.
    v_times <- dg_env$v_times
    v <- dg_env$v
    vw <- dg_env$vw
    #vc <- dg_env$vc
    w_eq <- dg_env$w_eq
    w_weq <- dg_env$w_weq
    w <- dg_env$w
  }
  if (doPlot){
    if (is.null(pdf_name) == FALSE) {
      pdf(file = pdf_name)
    }
    plot (v_times, get_y_axis(v_times), type = "n", xlab="Time", ylab="CDF")
    points (v_times, w, col="blue", type="l")
    points (v_times, v, col="green", type="l")
    #points (v_times, vc, col="red", type="l")
    points (v_times, w_eq, col="purple", type="l")
    points (v_times, vw, col="red", type="l")
    points (v_times, w_weq, col="black", type="l")
    
    legend("right", c("Simulation", "Equation", "Sim_Eq", "Eq_w_dist", "Sim_eq_w_dist"), col=c("blue", "green", "purple", "red", "black"), lty=1)
    
    if (is.null(pdf_name) == FALSE) {
      dev.off()
    }
  }
}