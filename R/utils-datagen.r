# file utils generate dis_op
# allowing more complex latency models

# Old get_tot
# Assumes that all nodes have the same Latency Model
# and now it should be the default behaviour of the new get_tot
#get_transmission_operation_times <- function(number_of_operations, config )
#{
#  val <- sample(c( rpareto(round(config$p_rpareto * number_of_operations), config$pareto_shape, config$pareto_scale ), rexp (round((1-config$p_rpareto) * number_of_operations), rate = config$len_lambda)))
#  
#  return(val)
#}

# But still respecting a hybrid of pareto-exponential
func_one_config_per_replica <- function(n, config) {
  
  # We keep the same value outside for the sake of compatibility
  # maybe we shouldnt.
  n <- n/config$num_repl
  sapply(1:config$num_repl, function(i){
    pr <- config$p_rpareto[i]
    psh <- config$pareto_shape[i]
    psc <- config$pareto_scale[i]
    pe <- 1 - pr
    l <- config$len_lambda[i]
    
    val_repl <- sample(c( rpareto(round(pr * n), psh, psc ), rexp (round(pe * n), rate = l)))
    
    return (val_repl)
    
  }) -> val 
  return (val)
}

get_transmission_operation_times <- function(n, config, f = NULL){
  func_default <- function(n, config){
    val <- sample(
                  c( rpareto1(round(config$p_rpareto * n), config$pareto_shape, config$pareto_scale ),
                     rexp (round((1-config$p_rpareto) * n), rate = config$len_lambda)
                  )
                 )
    return (val)
  }
  if (is.null(f)){
    f <- func_default
  }

  return (f(n, config))
}
