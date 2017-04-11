source("./R/utils.r")
source("./R/utils-pbcl.r")
#
# This script plots diferent combinations of lambda and replica with a constant number of rate
# This should show if equivalent lambda-replicas tuplets behave in the same way.
# 


ptm1 <- proc.time()[3]

# How many times we repeat the exp to get the mean
rep_iter <- 20

# each lambda-replicas tuplets is in the same position
# One combination of lambdas and rates

#plot_title <- "Lambdas"
#v_tags <- c("4","2","1","0.5","0.2","0.1")
#v_lambdas <- c(4,2,1,0.5,0.2,0.1)
#rate_op <- 10
#num_repl <- 5
#
#num_elements <- length(v_lambdas)

plot_title <- "Rate of Operations"
v_tags <- c("500-t1","500-t5","500-t10","500-t20")
v_rate_op <- c(500,500,500,500)
v_time_exp <- c(1,5,10,20)
lambda <- 0.1
num_repl <- 5

num_elements <- length(v_tags)

###########################

v_times <- seq(0, 120, 3)
l_data_sim <- NULL
l_data_sim_dnc <- NULL

for(j in 1:num_elements){

	mean_sim <- rep.int(0, length(v_times))
	mean_sim_dnc <- rep.int(0, length(v_times))

	for (iter in 1:rep_iter){

		#Using default scenario values
		config <- get_scenario(p_rpareto=0, len_lambda=lambda, time_exp=v_time_exp[j], rate_op=v_rate_op[j], num_repl=num_repl)

		m_data <- process_scenario(config, f_dc=matrix_causal_delivery_time_bt, f_dnc=matrix_delivery_time)
		
		d_dc <- get_operation_delivery_time (m_data$dc)
		d_dnc<- get_operation_delivery_time (m_data$dnc)

		sim_prob <- sim_prob_time(v_times, d_dc)
		sim_prob_dnc <- sim_prob_time(v_times, d_dnc)

		#eq_prob <- eq_prob_time( v_times, give_dependent_op(m_data$dc), m_data, config)


		mean_sim <- mean_sim + sim_prob
		mean_sim_dnc <- mean_sim_dnc + sim_prob_dnc
		#mean_eq <- mean_eq + eq_prob
	}

	#l_data_eq[j] <- list(mean_eq/rep_iter)
	l_data_sim[j] <- list(mean_sim/rep_iter)
	l_data_sim_dnc[j] <- list(mean_sim_dnc/rep_iter)
}




###########################
# Plotting 
###########################

x <- v_times
# no se perq cal el -1.
y <- seq(0, 0.99999999, 1/(length(v_times)) )
pdf("Rplots_6Rate_red_and_blues.pdf")
	plot(x, y, type="n", xlab="Time", ylab="CDF")
	legend(90, 0.37, v_tags, title=plot_title, lty=rep.int(1,num_elements), box.lwd=0, lwd=rep.int(1,num_elements), col="black", pch=1:num_elements, box.col="white")

	points(v_times, l_data_sim_dnc[[1]], type="l", col="red")

	for (j in 1:num_elements){
		points(v_times, l_data_sim[[j]], type="o", col="blue", pch=j)
		#points(v_times, l_data_eq[[j]], type="o", col="red", pch=j)
	}
	#title(main="Causal Latency", sub="Comparison between Prediction and Simulation")
	legend(89, 0.5, c("Causality","No Causality"), box.lwd=0, lty=c(1,1), lwd=c(2.5,2.5), col=c("blue", "red"), box.col="white")
dev.off()
###########################
# End Plotting
###########################

printf(">>> Time Elapsed: %.3f\n", proc.time()[3] - ptm1)