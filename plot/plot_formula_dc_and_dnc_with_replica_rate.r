source("./R/utils.r")
source("./R/utils-pbcl.r")
#
# This script plots diferent combinations of lambda and rate with a constant number of replicas
# This should show if equivalent lambda-rate tuplets behave in the same way.
# 


ptm1 <- proc.time()[3]

# How many times we repeat the exp to get the mean
rep_iter <- 10


#v_tags <- c("rate 0.4 - n 2","rate 0.6 - n 3","rate 1 - n 5","rate 1.4 - n 7","rate 2 - n 10")
#len_lambda <- 0.1
#v_rate_op <- c(1,3/4,5/8,7/12)
#v_num_repl <- c(2,3,5,7)
#num_elements <- length(v_num_repl)

v_tags <- c("rate 1","rate 10","rate 20","rate 30","rate 40","rate 50","rate 100")
len_lambda <- 0.5
v_rate_op <- c(1,10,20,30,40,50,100)
num_repl <- 5
num_elements <- length(v_rate_op)




v_times <- seq(0, 25, 1)
l_data_sim <- NULL
l_data_dnc_sim <- NULL
l_data_eq <- NULL

for(j in 1:num_elements){

	mean_sim <- rep.int(0, length(v_times))
	mean_eq <- rep.int(0, length(v_times))
	mean_dnc_sim <- rep.int(0, length(v_times))
	
	for (iter in 1:rep_iter){

		#Using default scenario values
		config <- get_scenario(p_rpareto=0, len_lambda=len_lambda, rate_op=v_rate_op[j], num_repl=num_repl)

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
# Save 
###########################

# We have to save both the data and the configuration
# the file naming should be better
save(v_times, v_tags, len_lambda, v_rate_op, num_repl, num_elements, l_data_dnc_sim, l_data_eq, l_data_sim, file="rate_dc_dnc_2.rdata")

###########################
# End_Save 
###########################


###########################
# Plotting 
###########################

x <- v_times
# no se perq cal el -1.
y <- get_y_axis(v_times)
plot(x, y, type="n", xlab="Time", ylab="CDF")

# Or we could compute the average of all num_elements for the green one.
points(v_times, l_data_dnc_sim[[1]], type="l", col="green")

for (j in 1:num_elements){
	points(v_times, l_data_sim[[j]], type="o", col="red", pch=j)
	points(v_times, l_data_eq[[j]], type="o", col="blue", pch=j)
}
#title(main="Causal Latency", sub="Comparison between Prediction and Simulation")
legend("bottomright", c("Prediction","Simulation","dnc",v_tags), lty=c(1,1,1,rep.int(1,num_elements)), lwd=c(2.5,2.5,2.5,rep.int(1,num_elements)), col=c("blue","red","green",rep("black", num_elements)), pch=c(0,0,0,1:num_elements), pt.cex=c(0,0,0,rep.int(1, num_elements)))

###########################
# End Plotting
###########################

printf(">>> Time Elapsed: %.3f\n", proc.time()[3] - ptm1)