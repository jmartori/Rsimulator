source("./tests/utils-test.r")

source("./R/utils-base.r")
source("./R/utils-pbcl.r")
source("./R/utils.r")
source("./R/utils-latmod.r")

eq_no_order <- function(r=10, l=1, t=0, n=5)
{
  return ( exp_snd(t, l) )
}


sim_no_order <- function(r=10, l=1, t=0, n=5, iter=2500) 
{
  pad <- round(0.1*iter)
  config <- get_scenario(p_rpareto=0, len_lambda=l, rate_op=(r*n), window_op=iter, padding_op=pad, num_repl = n)
  data <- process_scenario(config, f_dc=matrix_causal_delivery_time_bt, f_dnc=matrix_delivery_time)
  
  d_dnc <- get_operation_delivery_time(data$dnc[(pad+1):(pad+iter),])
  sim_prob <- sim_prob_time(t, d_dnc)
  
  return (sim_prob)
}