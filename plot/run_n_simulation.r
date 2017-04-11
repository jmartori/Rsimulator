# Runs the simulation n times and returns a list with all the m_data
# ... is for the configuration of the simulation.

source("./R/utils-base.r")
source("./R/utils-pbcl.r")
source("./R/utils.r")
source("./R/utils-latmod.r")

source("./tests/eq_causal_order.r")


run_n_simulations <- function(n = 1, t=seq(0,30,0.5), ...) {
  lm_data <- list()
  config <- NULL
  for (iter in 1:n){  
    config <- get_scenario(...)
    
    data <- process_scenario(config, f_dc=matrix_causal_delivery_time_bt, f_dnc=matrix_delivery_time)
    
    d_dc <- get_operation_delivery_time(data$dc)
    sim_prob <- sim_prob_time(t, d_dc)
    data$sim_prob <- sim_prob
    
    # n should be acording to t(p)r to save some computing time.
    vp_dist <- get_prob_deliv_remote_op_2(data$dc, data$dnc, num_repl = config$num_repl, v_times = t) #seq(0, n, 1/rate)
    vw <- eq_causal_order_w_dist(vp_dist = vp_dist, r=(config$rate_op)/config$num_repl, l=config$len_lambda, t=t, n=config$num_repl)
    
    data$vw <- vw
    data$vp_dist <- vp_dist
        
    lm_data[[iter]] <- data
  }
  
  lm_data.vp_dist <- sapply(1:length(lm_data), function(i) lm_data[[i]]$vp_dist)
  vp_dist <- apply(lm_data.vp_dist, 1, mean)
  vp_dist_norm <- as.vector(sapply(1:length(t), function(i) rep(vp_dist[i], config$rate_op/config$num_repl)))
  vw <- eq_causal_order_w_dist(vp_dist = vp_dist_norm, r=(config$rate_op)/config$num_repl, l=config$len_lambda, t=t, n=config$num_repl)
  
  lm_data.sim_prob <-sapply(1:length(lm_data), function(i) lm_data[[i]]$sim_prob)
  sim_prob <- apply(lm_data.sim_prob, 1, mean)
  
  return(list(lm_data=lm_data, sim_prob=sim_prob, vp_dist=vp_dist, vw=vw, config=config))
}


rmsd <- function(yh, y) {
  n <- length(y)
  return(sqrt(sum((yh-y)^2)/n))
}

nrmsd <- function(yh, y) {
  n <- length(yh)
  return(rmsd(yh, y)/(max(yh) - min(yh)))
}
