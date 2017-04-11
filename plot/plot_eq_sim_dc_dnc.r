source("./R/utils.r")
source("./R/utils-pbcl.r")
#
# This script plots diferent combinations of lambda and replica with a constant number of rate
# This should show if equivalent lambda-replicas tuplets behave in the same way.
# 


ptm1 <- proc.time()[3]

# How many times we repeat the exp to get the mean
rep_iter <- 1

# each lambda-replicas tuplets is in the same position
# One combination of lambdas and rates
v_tags <- c("\u03bb 0.1","\u03bb 0.2","\u03bb 0.5","\u03bb 1","\u03bb 2","\u03bb 4")
v_lambdas <- c(0.1,0.2,0.5,1,2,4)
rate_op <- 2/5
num_repl <- 5

num_elements <- length(v_lambdas)



v_times <- seq(0, 40, 1)
l_data_sim <- NULL
l_data_dnc_sim <- NULL
l_data_eq <- NULL

for(j in 1:num_elements){

	mean_sim <- rep.int(0, length(v_times))
	mean_dnc_sim <- rep.int(0, length(v_times))
	mean_eq <- rep.int(0, length(v_times))

	for (iter in 1:rep_iter){

		#Using default scenario values
		config <- get_scenario(p_rpareto=0, len_lambda=v_lambdas[j], rate_op=rate_op, num_repl=num_repl)

		m_data <- process_scenario(config, f_dc=matrix_causal_delivery_time_bt, f_dnc=matrix_delivery_time)
#		m_data <- process_scenario(config, f_dnc=matrix_delivery_time)		

		d_dc <- get_operation_delivery_time (m_data$dc)
		d_dnc <- get_operation_delivery_time (m_data$dnc)

		sim_prob <- sim_prob_time(v_times, d_dc)
#		dnc_sim_prob <- sim_prob_time(v_times, d_dnc)
		eq_prob <- eq_prob_time( v_times, give_dependent_op(m_data$dc), m_data, config)


		mean_sim <- mean_sim + sim_prob
#		mean_dnc_sim <- mean_dnc_sim + dnc_sim_prob
		mean_eq <- mean_eq + eq_prob
	}

	l_data_eq[j] <- list(mean_eq/rep_iter)
	l_data_sim[j] <- list(mean_sim/rep_iter)
#	l_data_dnc_sim[j] <- list(mean_dnc_sim/rep_iter)
}

###########################
# Save 
###########################

# We have to save both the data and the configuration
# the file naming should be better
save(v_times, v_tags, v_lambdas, rate_op, num_repl, num_elements, l_data_eq, l_data_sim, file="lambda_dc_dnc.rdata")

###########################
# End_Save 
###########################



###########################
# Plotting 
###########################

x <- v_times
# no se perq cal el -1.
y <- get_y_axis(v_times)

plot(x, y, type="n")#, xlab="Time since message was sent", ylab="Prob of message delivered")

for (j in 1:num_elements){
	points(v_times, l_data_sim[[j]], type="o", col="red", pch=j)
	points(v_times, l_data_eq[[j]], type="o", col="blue", pch=j)
#	points(v_times, l_data_dnc_sim[[j]], type="o", col="green", pch=j)
}
#title(main="Causal Latency", sub="Comparison between Prediction and Simulation")
#legend(25, 0.43, c("Prediction","Simulation","No Causality"), lty=c(1,1,1), lwd=c(2.5,2.5,2.5), col=c("blue", "red", "green"), box.col="white")
legend(25, 0.43, c("Prediction","Simulation"), lty=c(1,1), lwd=c(2.5,2.5), col=c("blue", "red"))
legend(26, 0.28, v_tags, lty=rep.int(1,num_elements), lwd=rep.int(1,num_elements), col="black", pch=1:num_elements)

###########################
# End Plotting
###########################

printf(">>> Time Elapsed: %.3f\n", proc.time()[3] - ptm1)