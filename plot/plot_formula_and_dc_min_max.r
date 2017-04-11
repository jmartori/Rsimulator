source("./R/utils.r")
source("./R/utils-pbcl.r")

ptm1 <- proc.time()[3]

# How many times we repeat the exp to get the mean
rep_iter <- 10


#mean_sim <- 0
#mean_eq <- 0

v_times <- seq(0, 120, 4)
mean_sim <- rep.int(0, length(v_times))
mean_eq <- rep.int(0, length(v_times))
min_sim <- rep.int(1, length(v_times)) 	# i default it to the highest value
max_sim <- rep.int(0, length(v_times)) 	# i default it to the lowest value


for (iter in 1:rep_iter){

	#Using default scenario values
	config <- get_scenario(p_rpareto=0)

	m_data <- process_scenario(config, f_dc=matrix_causal_delivery_time_bt, f_dnc=matrix_delivery_time)
	
	d_dc <- get_operation_delivery_time (m_data$dc)

	sim_prob <- sim_prob_time(v_times, d_dc)
	eq_prob <- eq_prob_time( v_times, give_dependent_op(m_data$dc), m_data, config)


	mean_sim <- mean_sim + sim_prob
	mean_eq <- mean_eq + eq_prob
	
	min_sim <- apply ( matrix(c(min_sim, sim_prob), ncol=2), 1, min)
	max_sim <- apply ( matrix(c(max_sim, sim_prob), ncol=2), 1, max)
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
points(v_times, min_sim, type="l", col="green")
points(v_times, max_sim, type="l", col="green")

title(main="Causal Latency", sub="Comparison between Prediction and Simulation")
legend(80, 0.5, c("Prediction","Simulation","Sim_Min_Max"), lty=c(1,1,1), lwd=c(2.5,2.5,2.5), col=c("blue", "red", "green") )

###########################
# End Plotting
###########################

printf(">>> Time Elapsed: %.3f\n", proc.time()[3] - ptm1)