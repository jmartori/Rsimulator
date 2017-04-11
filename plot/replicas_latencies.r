time0 <- proc.time()[3]
source("R/utils.r")
source("load_pbs_config.r")

run <- function(k=1, num_repl = 5, rate_op = 10, v_times = NULL, n = 50, fn = sprintf("data/pbs_%.0f_iter_%.0f_c1_10-1-01_rate_50.rdata", k, n), window_op = 2000){
  
  config <- load_pbs_config_list(k)
  

  r_data <- replicate(n=n, expr = process_scenario(get_scenario(num_repl = num_repl, rate_op = (num_repl*rate_op), c1_dt=config$c1_01, len_lambda = config$lambdas, p_rpareto = config$ppareto, pareto_shape = config$shape, pareto_scale = config$scale, window_op = window_op), f_dc=matrix_causal_delivery_time_bt, f_dc_er=NULL, f_dnc=matrix_delivery_time, f_fifo=matrix_fifo_delivery_time))
>>>>>>> b3367a6e65e8acbbaf2dd1fc0c31a1ec0a1e4bdd
  
  max_times <- round(max(apply( r_data, 2, function(mi) get_operation_delivery_time(mi$dc))))
  
  # 10k so the plot has a 10k hundred points, per line # The more points the better the plot is!
  if (is.null(v_times)) v_times <- seq(0, max_times , max_times/10000)
  ids <- (round(0.1*window_op)):(round((1.1*window_op)))
  sp.dc <- apply( r_data, 2, function(mi) sim_prob_time_2(v_times, get_operation_delivery_time(mi$dc[ids,])))
  sp.dnc <- apply( r_data, 2, function(mi) sim_prob_time_2(v_times, get_operation_delivery_time(mi$dnc[ids,])))
  sp.fifo <- apply( r_data, 2, function(mi) sim_prob_time_2(v_times, get_operation_delivery_time(mi$fifo[ids,])))
  
  m <- cbind(sp.dc, sp.dnc, sp.fifo)
  
  dc <- list(min=apply(m[,(1):(n)], 1, min), max=apply(m[,(1):(n)], 1, max), mean=apply(m[,(1):(n)], 1, mean), median=apply(m[,(1):(n)], 1, median) )
  dnc <- list(min=apply(m[,(n+1):(2*n)], 1, min), max=apply(m[,(n+1):(2*n)], 1, max), mean=apply(m[,(n+1):(2*n)], 1, mean), median=apply(m[,(n+1):(2*n)], 1, median) )
  fifo <- list(min=apply(m[,(2*n+1):(3*n)], 1, min), max=apply(m[,(2*n+1):(3*n)], 1, max), mean=apply(m[,(2*n+1):(3*n)], 1, mean), median=apply(m[,(2*n+1):(3*n)], 1, median) )
  
  if (interactive() == TRUE) {
    return(list(v_times = v_times, sp.dc = sp.dc, sp.dnc = sp.dnc, dc = dc, dnc = dnc, fifo = fifo))
  } else{
    save(v_times, m, r_data, dnc, dc, fifo, file=fn)
  }
}

############ For Launch from Rscript
if (interactive() == FALSE){
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
}
