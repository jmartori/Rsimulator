source("R/utils.r")
source("load_pbs_config.r")

run <- function(k=1){
  fn = sprintf("data/pbs_%d_iter_25.rdata",k)
  config <- load_pbs_config_list(k)
  window_op = 10000
  r_data <- replicate(n=25, expr = process_scenario(get_scenario(len_lambda = config$lambdas, p_rpareto = config$ppareto, pareto_shape = config$shape, pareto_scale = config$scale, window_op = window_op)
                                                    , f_dc=matrix_causal_delivery_time_bt, f_dc_er=matrix_causal_delivery_time_bt_with_error_recovery, f_dnc=matrix_delivery_time))
  max_times <- round(max(apply( r_data, 2, function(mi) get_operation_delivery_time(mi$dc))))
  # 200 so the plot has a 200 hundred points, per line
  v_times <- seq(0, max_times , max_times/200)
  sp.dc <- apply( r_data, 2, function(mi) sim_prob_time_2(v_times, get_operation_delivery_time(mi$dc)))
  sp.dnc <- apply( r_data, 2, function(mi) sim_prob_time_2(v_times, get_operation_delivery_time(mi$dnc)))
  sp.dc_er <- apply( r_data, 2, function(mi) sim_prob_time_2(v_times, get_operation_delivery_time(mi$dc_er$data)))
  
  m <- cbind(sp.dc, sp.dnc, sp.dc_er)
  save(v_times, m, r_data, file=fn)
}

############ For Launch from Rscript
if (length(args)==0) {
  stop("Come on man, give me something!", call.=FALSE)
}
args <- commandArgs(trailingOnly = TRUE)
n <- as.numeric(args[1])

run(n)
