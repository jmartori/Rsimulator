source("./R/utils.r")
source("./load_pbs_config.r")


launch_with_config <- function (n=8, print_time=FALSE, num_repl=5, rate_op=25, window_op=10000){
  if (print_time) ptm0 <- proc.time()[3]
  pbs_config <- load_pbs_config_list(n)
  	len_lambda <- pbs_config$lambdas 
  	p_rpareto  <- pbs_config$ppareto
  	pareto_shape <- pbs_config$shape
  	pareto_scale <- pbs_config$scale

  config <- get_scenario(num_repl=num_repl, rate_op=rate_op, padding_op=1000, window_op=window_op, len_lambda=len_lambda, p_rpareto=p_rpareto, pareto_shape=pareto_shape, pareto_scale=pareto_scale, c1_dt=0, c2_dt=120) 
  #m_data <- process_scenario(config, f_dc=matrix_causal_delivery_time_bt, f_dnc=matrix_delivery_time, f_dc_er=matrix_causal_delivery_time_bt_with_error_recovery)
  m_data <- process_scenario(config, f_dc=matrix_causal_delivery_time_bt, f_dnc=matrix_delivery_time)

  if (print_time) printf(">>> %f\n", proc.time()[3] - ptm0)
  return(m_data)
}
