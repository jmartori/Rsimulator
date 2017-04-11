source("./R/utils.r")
source("./R/utils-pbcl.r")

ptm1 <- proc.time()[3]

# How many times we repeat the exp to get the mean
rep_iter <- 10


mean_sim <- 0
mean_eq <- 0

v_times <- seq(0, 120, 4)

for (iter in 1:rep_iter){

	#Using default scenario values
	config <- get_scenario(p_rpareto=0)

	m_data <- process_scenario(config, f_dc=matrix_causal_delivery_time_bt, f_dnc=matrix_delivery_time)
	
	d_dc <- get_operation_delivery_time (m_data$dc)

	sim_prob <- sim_prob_time(v_times, d_dc)
	eq_prob <- eq_prob_time( v_times, give_dependent_op(m_data$dc), m_data, config)


	mean_sim <- mean_sim + sim_prob
	mean_eq <- mean_eq + eq_prob
}

mean_sim <- mean_sim/rep_iter
mean_eq <- mean_eq/rep_iter



###########################
# Plotting 
###########################

x <- v_times
# no se perq cal el -1.
y <- seq(0, 1, 1/(length(v_times)-1) )
plot(x, y, type="n", xlab="Units of Time", ylab="Prob of message delivered")

points(v_times, mean_sim, type="l", col="red")
points(v_times, mean_eq, type="l", col="blue")

title(main="Causal Latency", sub="Comparison between Prediction and Simulation")
legend(90, 0.5, c("Prediction","Simulation"), lty=c(1,1), lwd=c(2.5,2.5), col=c("blue", "red") )

###########################
# End Plotting
###########################

printf(">>> Time Elapsed: %.3f\n", proc.time()[3] - ptm1)