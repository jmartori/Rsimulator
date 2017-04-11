source("./R/utils.r")
source("./R/utils-pbcl.r")
#
# This script plots diferent combinations of lambda and replica with a constant number of rate
# This should show if equivalent lambda-replicas tuplets behave in the same way.
# 


ptm1 <- proc.time()[3]

# How many times we repeat the exp to get the mean
rep_iter <- 100

# each lambda-replicas tuplets is in the same position
# One combination of lambdas and rates
v_tags <- c("\u03bb 0.4 - n 2","\u03bb 0.6 - n 3","\u03bb 1 - n 5","\u03bb 1.4 - n 7","\u03bb 2 - n 10")
len_lambda <- 0.1
v_rate_op <- c()
v_num_repl <- c()
num_elements <- 5



v_times <- seq(0, 40, 1)
l_data_sim <- NULL
l_data_eq <- NULL

for(j in 1:num_elements){

	mean_sim <- rep.int(0, length(v_times))
	mean_eq <- rep.int(0, length(v_times))

	for (iter in 1:rep_iter){

		#Using default scenario values
		config <- get_scenario(p_rpareto=0, len_lambda=len_lambda, rate_op=v_rate_op[j], num_repl=v_num_repl[j])

		m_data <- process_scenario(config, f_dc=matrix_causal_delivery_time_bt, f_dnc=matrix_delivery_time)
		
		d_dc <- get_operation_delivery_time (m_data$dc)

		sim_prob <- sim_prob_time(v_times, d_dc)
		eq_prob <- eq_prob_time( v_times, give_dependent_op(m_data$dc), m_data, config)


		mean_sim <- mean_sim + sim_prob
		mean_eq <- mean_eq + eq_prob
	}

	l_data_eq[j] <- list(mean_eq/rep_iter)
	l_data_sim[j] <- list(mean_sim/rep_iter)
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
legend(25, 0.5, c("Prediction","Simulation"), lty=c(1,1), lwd=c(2.5,2.5), col=c("blue", "red"), box.col="white")
legend(26, 0.37, v_tags, lty=rep.int(1,num_elements), lwd=rep.int(1,num_elements), col="black", pch=1:num_elements, box.col="white")

###########################
# End Plotting
###########################

printf(">>> Time Elapsed: %.3f\n", proc.time()[3] - ptm1)