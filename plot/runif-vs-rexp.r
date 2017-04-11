source("R/utils.r")
source("load_pbs_config.r")

run <- function(k=1, num_repl = 5, rate_op = 10, v_times = NULL, n = 50, method = c("exp_sbs", "par_sbs", "exp", "par", "unif", "unif_sbs", "static")){
  
  config <- load_pbs_config_list(k)
  window_op = 2000
  
  r_data.unif <- replicate(n=n, expr = process_scenario(get_scenario(t_op_method = "unif", num_repl = num_repl, rate_op = rate_op, c1_dt=config$c1_01, len_lambda = config$lambdas, p_rpareto = config$ppareto, pareto_shape = config$shape, pareto_scale = config$scale, window_op = window_op), f_dc=matrix_causal_delivery_time_bt, f_dc_er=NULL, f_dnc=matrix_delivery_time))
  r_data.exp <- replicate(n=n, expr = process_scenario(get_scenario(t_op_method  = method, num_repl = num_repl, rate_op = rate_op, c1_dt=config$c1_01, len_lambda = config$lambdas, p_rpareto = config$ppareto, pareto_shape = config$shape, pareto_scale = config$scale, window_op = window_op), f_dc=matrix_causal_delivery_time_bt, f_dc_er=NULL, f_dnc=matrix_delivery_time))
  
  
  
  max_times.unif <- round(max(apply( r_data.unif, 2, function(mi) get_operation_delivery_time(mi$dc))))
  max_times.exp <- round(max(apply( r_data.exp, 2, function(mi) get_operation_delivery_time(mi$dc))))
  max_times <- max(max_times.unif, max_times.exp)
  
  # 10k so the plot has a 10k hundred points, per line # The more points the better the plot is!
  if (is.null(v_times)) v_times <- seq(0, max_times , max_times/10000)
  
  sp.dc.unif <- apply( r_data.unif, 2, function(mi) sim_prob_time_2(v_times, get_operation_delivery_time(mi$dc)))
  sp.dnc.unif <- apply( r_data.unif, 2, function(mi) sim_prob_time_2(v_times, get_operation_delivery_time(mi$dnc)))
  
  sp.dc.exp <- apply( r_data.exp, 2, function(mi) sim_prob_time_2(v_times, get_operation_delivery_time(mi$dc)))
  sp.dnc.exp <- apply( r_data.exp, 2, function(mi) sim_prob_time_2(v_times, get_operation_delivery_time(mi$dnc)))
  
  m <- cbind(sp.dc.unif, sp.dnc.unif, sp.dc.exp, sp.dnc.exp)
  
  dc.unif <- list(min=apply(m[,(1):(n)], 1, min), max=apply(m[,(1):(n)], 1, max), mean=apply(m[,(1):(n)], 1, mean) )
  dnc.unif <- list(min=apply(m[,(n+1):(2*n)], 1, min), max=apply(m[,(n+1):(2*n)], 1, max), mean=apply(m[,(n+1):(2*n)], 1, mean) )
  dc.exp <- list(min=apply(m[,(2*n+1):(3*n)], 1, min), max=apply(m[,(2*n+1):(3*n)], 1, max), mean=apply(m[,(2*n+1):(3*n)], 1, mean) )
  dnc.exp <- list(min=apply(m[,(3*n+1):(4*n)], 1, min), max=apply(m[,(3*n+1):(4*n)], 1, max), mean=apply(m[,(3*n+1):(4*n)], 1, mean) )
  
  return(list(v_times = v_times, sp.dc.unif = sp.dc.unif, sp.dnc.unif = sp.dnc.unif, sp.dc.exp = sp.dc.exp, sp.dnc.exp = sp.dnc.exp, dc.exp = dc.exp, dnc.exp = dnc.exp, dc.unif = dc.unif, dnc.unif = dnc.unif))
}