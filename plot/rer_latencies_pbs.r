time0 <- proc.time()[3]
source("R/utils.r")
source("load_pbs_config.r")

run <- function(k=1){
  n <- 50
  fn <- sprintf("data/pbs_%d_iter_%d_c1_10-1-01_rate_50.rdata", k, n)
  config <- load_pbs_config_list(k)
  window_op = 2000
  r_data.c01 <- replicate(n=n, expr = process_scenario(get_scenario(rate_op = 50, c1_dt=config$c1_01, len_lambda = config$lambdas, p_rpareto = config$ppareto, pareto_shape = config$shape, pareto_scale = config$scale, window_op = window_op)
                                                    , f_dc=matrix_causal_delivery_time_bt, f_dc_er=matrix_causal_delivery_time_bt_with_error_recovery, f_dnc=matrix_delivery_time))
  r_data.c1 <- replicate(n=n, expr = process_scenario(get_scenario(rate_op = 50, c1_dt=config$c1_1, len_lambda = config$lambdas, p_rpareto = config$ppareto, pareto_shape = config$shape, pareto_scale = config$scale, window_op = window_op)
                                                    , f_dc=matrix_causal_delivery_time_bt, f_dc_er=matrix_causal_delivery_time_bt_with_error_recovery, f_dnc=matrix_delivery_time))
  r_data.c10 <- replicate(n=n, expr = process_scenario(get_scenario(rate_op = 50, c1_dt=config$c1_10, len_lambda = config$lambdas, p_rpareto = config$ppareto, pareto_shape = config$shape, pareto_scale = config$scale, window_op = window_op)
                                                    , f_dc=matrix_causal_delivery_time_bt, f_dc_er=matrix_causal_delivery_time_bt_with_error_recovery, f_dnc=matrix_delivery_time))
  
  max_times <- max(round(max(apply( r_data.c01, 2, function(mi) get_operation_delivery_time(mi$dc)))),
                   round(max(apply( r_data.c1, 2, function(mi) get_operation_delivery_time(mi$dc)))),
                   round(max(apply( r_data.c10, 2, function(mi) get_operation_delivery_time(mi$dc))))
                   )
  
  # 10k so the plot has a 10k hundred points, per line # The more points the better the plot is!
  v_times <- seq(0, max_times , max_times/10000)
  
  sp.dc <- apply( r_data.c10, 2, function(mi) sim_prob_time_2(v_times, get_operation_delivery_time(mi$dc)))
  sp.dnc <- apply( r_data.c10, 2, function(mi) sim_prob_time_2(v_times, get_operation_delivery_time(mi$dnc)))
  
  sp.dc_er.c10 <- apply( r_data.c10, 2, function(mi) sim_prob_time_2(v_times, get_operation_delivery_time(mi$dc_er$data)))
  sp.dc_er.c1 <- apply( r_data.c1, 2, function(mi) sim_prob_time_2(v_times, get_operation_delivery_time(mi$dc_er$data)))
  sp.dc_er.c01 <- apply( r_data.c01, 2, function(mi) sim_prob_time_2(v_times, get_operation_delivery_time(mi$dc_er$data)))
  
  m <- cbind(sp.dc, sp.dnc, sp.dc_er.c10, sp.dc_er.c1, sp.dc_er.c01)
  
  dc <- list(min=apply(m[,(1):(n)], 1, min), max=apply(m[,(1):(n)], 1, max), mean=apply(m[,(1):(n)], 1, mean) )
  dnc <- list(min=apply(m[,(n+1):(2*n)], 1, min), max=apply(m[,(n+1):(2*n)], 1, max), mean=apply(m[,(n+1):(2*n)], 1, mean) )
  dc_er.c10 <- list(min=apply(m[,(2*n+1):(3*n)], 1, min), max=apply(m[,(2*n+1):(3*n)], 1, max), mean=apply(m[,(2*n+1):(3*n)], 1, mean) )
  dc_er.c1  <- list(min=apply(m[,(3*n+1):(4*n)], 1, min), max=apply(m[,(3*n+1):(4*n)], 1, max), mean=apply(m[,(3*n+1):(4*n)], 1, mean) )
  dc_er.c01 <- list(min=apply(m[,(4*n+1):(5*n)], 1, min), max=apply(m[,(4*n+1):(5*n)], 1, max), mean=apply(m[,(4*n+1):(5*n)], 1, mean) )
  
  
  save(v_times, m, r_data.c10, r_data.c1, r_data.c01, dnc, dc, dc_er.c10, dc_er.c1, dc_er.c01, file=fn)
}

############ For Launch from Rscript
if (length(args)==0) {
  stop("Come on man, give me something!", call.=FALSE)
}
args <- commandArgs(trailingOnly = TRUE)
n <- as.numeric(args[1])

run(n)


# Out we go
time1 <- proc.time()[3]
system(sprintf("notify-send  '%d is done in %f' ", n, time1 - time0))

cat(sprintf("%d is done in %f \n", n, time1 - time0))

