source("./R/utils.r")
source("./R/utils-pbcl.r")
#
# This script plots diferent combinations of lambda and rate with a constant number of replicas
# This should show if equivalent lambda-rate tuplets behave in the same way.
# 


ptm1 <- proc.time()[3]

# How many times we repeat the exp to get the mean
rep_iter <- 100

# each lambda-rate tuplets is in the same position
# One combination of lambdas and rates
#v_tags <- c("\u03bb - rate 0.1","\u03bb - rate 0.2","\u03bb - rate 0.3","\u03bb - rate 0.4","\u03bb - rate 0.5")
#v_lambdas <- c(0.1,0.2,0.3,0.4,0.5)
#v_rates   <- c(0.1,0.2,0.3,0.4,0.5)
#num_repl <- 5
#num_elements <- length(v_rates)

# Another Combination of Lambdas and rates
#v_tags <- c("\u03bb 2.5 - rate 0.4","\u03bb 1.25 - rate 0.2","\u03bb 0.25 - rate 0.04","\u03bb 5 - rate 0.8","\u03bb 0.625 - rate 0.1")
#v_lambdas <- c(5/2,5/4,1/4,5,5/8)
#v_rates   <- c(2/5,1/5,1/25,4/5,1/10)
#num_repl <- 2
#num_elements <- length(v_rates)

# Another Combination of Lambdas and rates
v_tags <- c("\u03bb 0.88 - rate 0.56","\u03bb 0.44 - rate 0.28","\u03bb 0.08 - rate 0.05","\u03bb 1.77 - rate 1.12","\u03bb 0.296 - rate 0.375")
v_lambdas <- c(8/9, 8/18, 8/90, 16/9, 8/27)
v_rates   <- c(9/8, 9/16, 9/80, 9/8, 9/24)
num_repl <- 10
num_elements <- length(v_rates)






v_times <- seq(0, 40, 1)
l_data_sim <- NULL
l_data_dnc_sim <- NULL
l_data_eq <- NULL

for(j in 1:num_elements){
  
  mean_sim <- rep.int(0, length(v_times))
  mean_eq <- rep.int(0, length(v_times))
  mean_dnc_sim <- rep.int(0, length(v_times))
  
  for (iter in 1:rep_iter){
    
    #Using default scenario values
    config <- get_scenario(p_rpareto=0, len_lambda=v_lambdas[j], rate_op=v_rates[j], num_repl=num_repl)
    
    m_data <- process_scenario(config, f_dc=matrix_causal_delivery_time_bt, f_dnc=matrix_delivery_time)
    
    d_dc <- get_operation_delivery_time (m_data$dc)
    d_dnc <- get_operation_delivery_time (m_data$dnc)
    
    sim_prob <- sim_prob_time(v_times, d_dc)
    dnc_sim_prob <- sim_prob_time(v_times, d_dnc)
    eq_prob <- eq_prob_time( v_times, give_dependent_op(m_data$dc), m_data, config)
    
    
    mean_sim <- mean_sim + sim_prob
    mean_dnc_sim <- mean_dnc_sim + dnc_sim_prob
    mean_eq <- mean_eq + eq_prob
  }
  
  l_data_eq[j] <- list(mean_eq/rep_iter)
  l_data_sim[j] <- list(mean_sim/rep_iter)
  l_data_dnc_sim[j] <- list(mean_dnc_sim/rep_iter)
}




###########################
# Plotting 
###########################

x <- v_times
# no se perq cal el -1.
y <- seq(0, 0.99999999, 1/(length(v_times)) )
plot(x, y, type="n", xlab="Units of Time", ylab="Prob of message delivered")

for (j in 1:num_elements){
  points(v_times, l_data_sim[[j]], type="o", col="red", pch=j)
  points(v_times, l_data_eq[[j]], type="o", col="blue", pch=j)
  points(v_times, l_data_dnc_sim[[j]], type="o", col="green", pch=j)
  
}
title(main="Causal Latency", sub="Comparison between Prediction and Simulation")
legend(25, 0.5, c("Prediction","Simulation","dnc"), lty=c(1,1,1), lwd=c(2.5,2.5,2.5), col=c("blue", "red","green"), box.col="white")
legend(26, 0.37, v_tags, lty=rep.int(1,num_elements), lwd=rep.int(1,num_elements), col="black", pch=1:num_elements, box.col="white")

###########################
# End Plotting
###########################

printf(">>> Time Elapsed: %.3f\n", proc.time()[3] - ptm1)