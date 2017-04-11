# 
# Given a configuration for the experiment and a number of iterations, it returns the latency matrix
# 
# Configuration parameters
time0 <- proc.time()[3]
source("./R/utils.r")
source("./load_pbs_config.r")




padding_op <- 100
window_op <- 1000

num_repl <- 5
rate_op <- 25




fp_error <- 0.05

#v_val_fp <- c(0.1, 0.01, 0.001)
#v_config <- 2:4

v_val_fp <- c(0.01, 0.001)
v_config <- 1


#c1_dt <- 10 # Not used while v_c1_dt exists
c1_dt <- 1
c2_dt <- 1


f_dc <- matrix_causal_delivery_time_bt
f_dnc <- matrix_delivery_time
f_fifo <- NULL
f_dc_er <- matrix_causal_delivery_time_bt_with_error_recovery
b_fifo <- TRUE



max_iterations <- 10

debug <- TRUE
# end config parameters
gl_iter <- 0


for (k_pbs_config in v_config){

	filename <- sprintf("data/pbs_%d_c1.rdata", k_pbs_config)
	c1_dt <- 0
	
	

	ml_data <- NULL
	ml_dc_er_log <- NULL
	# For each configuration we want to check.

	vv_fp <- NULL
	vv_c1_dt <- NULL


	for (k in 1:length(v_val_fp))
	{
		step <- 0
		a <- c1_dt
		b <- c1_dt+1

		c1_flag <- FALSE
		while (!c1_flag){
			pbs_config <- load_pbs_config_list(k_pbs_config)
			
			#len_lambda <- pbs_config$lambdas 
			len_lambda <- pbs_config$fake_lambdas # gotten using the get_equiv_lambda_pbs
			#p_rpareto  <- pbs_config$ppareto 		# if using fake_lambdas ppareto has to be 0
			p_rpareto  <- 0
			pareto_shape <- pbs_config$shape
			pareto_scale <- pbs_config$scale
	
			l_dc <- NULL
			l_fifo <- NULL
			l_dnc <- NULL
			l_dc_er <- NULL
			
			if (step == 0) {
				c1_dt <- b
			} else {
				c1_dt <- (a+b)/2
			}
			l_dc_er_log <- list(gain=NULL,fp=NULL,retries=NULL,first=NULL,improve=NULL,miss=NULL)
			v_iter_fp <- NULL
			l_config <- NULL
	
			for (iter in 1:max_iterations){
				time1 <- proc.time()[3]
				
				#config <- get_scenario(num_repl=num_repl, rate_op=rate_op, padding_op, window_op, len_lambda, p_rpareto, pareto_shape, pareto_scale, c1_dt, c2_dt) 
				config <- get_scenario(num_repl=num_repl, rate_op=rate_op, padding_op=padding_op, window_op=window_op, len_lambda=len_lambda, p_rpareto=p_rpareto, pareto_shape=pareto_shape, pareto_scale=pareto_scale, c1_dt=c1_dt) 

				m_data <- process_scenario(config, f_dc=f_dc, f_fifo=f_fifo, f_dc_er=f_dc_er, f_dnc=f_dnc, b_fifo=b_fifo)
	
				if ( is.null(f_dc) == FALSE) 	   l_dc <-  rbind(l_dc, get_operation_latency_time(m_data$dc[(padding_op+1):(padding_op+window_op),]))
				if ( is.null(f_dnc) == FALSE) 	  l_dnc <- rbind(l_dnc, get_operation_latency_time(m_data$dnc[(padding_op+1):(padding_op+window_op),]) )
				if ( is.null(f_fifo) == FALSE)   l_fifo <- rbind(l_fifo, get_operation_latency_time(m_data$fifo[(padding_op+1):(padding_op+window_op),]) )
				if ( is.null(f_dc_er) == FALSE) {
					l_dc_er <- rbind(l_dc_er, get_operation_latency_time(m_data$dc_er$data[(padding_op+1):(padding_op+window_op),]) )
					# test this
					l_dc_er_log[["retries"]] <- c(l_dc_er_log[["retries"]], m_data$dc_er$log$retries[(padding_op+1):(padding_op+window_op)])
					l_dc_er_log[["gain"]] <- c(l_dc_er_log[["gain"]], m_data$dc_er$log$gain[(padding_op+1):(padding_op+window_op)])
					l_dc_er_log[["fp"]] <- c(l_dc_er_log[["fp"]], m_data$dc_er$log$fp[(padding_op+1):(padding_op+window_op)])
					l_dc_er_log[["first"]] <- c(l_dc_er_log[["first"]], m_data$dc_er$log$first[(padding_op+1):(padding_op+window_op)])
					l_dc_er_log[["improve"]] <- c(l_dc_er_log[["improve"]], m_data$dc_er$log$improve_time[(padding_op+1):(padding_op+window_op)])
					l_dc_er_log[["miss"]] <- c(l_dc_er_log[["miss"]], m_data$dc_er$log$miss_op_count[(padding_op+1):(padding_op+window_op)])
				}
				fp <- mean(m_data$dc_er$log$fp[(padding_op+1):(padding_op+window_op)])
				v_iter_fp <- c(v_iter_fp, fp)
	
				#if (debug) printf("[%d] < Config %d> <Iter %d> %d %f - %f %f\n", k, k_pbs_config, iter, c1_dt, fp, proc.time()[3] - time0, proc.time()[3] - time1)
			}
			fp <- mean(v_iter_fp)
	
			if (fp < (v_val_fp[k]-(fp_error*v_val_fp[k]))) {
				if (step == 0){
					step <- 1
				} else {
					b <- c1_dt
				}
			}
			else{
				if (fp > (v_val_fp[k]+(fp_error*v_val_fp[k]))) {
					#c1_dt <- (c1_dt/2)+0.01
					if (step == 0){
						a <- b
						b <- 2*a
					} else {
						a <- c1_dt
					}
				}
				else{
					c1_flag <- TRUE
					vv_fp <- c(vv_fp, fp)
					vv_c1_dt <- c(vv_c1_dt, c1_dt)
					printf("<Iter %d> %d %f %f \n", gl_iter, k, fp, c1_dt)
				}
			}
			#printf("<Iter %d> %d %f %f \n", gl_iter, k, fp, c1_dt)
			if (gl_iter %% 50 == 0) {
				printf("\t\t\t\t <Iter %d> %d %d %f %f \n", gl_iter, step, k, fp, c1_dt)
			}

			gl_iter <- gl_iter + 1

			vv_fp <- c(vv_fp, mean(v_iter_fp))	
	
			ml_data[[k]] <- list(dc=l_dc, fifo=l_fifo, dnc=l_dnc, dc_er=l_dc_er)
			ml_dc_er_log[[k]] <- l_dc_er_log
			l_config[[k]] <- config
		}
	}
	
	#printf(" c = {%d, %d, %d}\n", v_c1_dt[which.min(abs(vv_fp - 0.1))], v_c1_dt[which.min(abs(vv_fp - 0.01))], v_c1_dt[which.min(abs(vv_fp - 0.001))])
	#printf("fp = {%f, %f, %f}\n", vv_fp[which.min(abs(vv_fp - 0.1))], vv_fp[which.min(abs(vv_fp - 0.01))], vv_fp[which.min(abs(vv_fp - 0.001))])

	save ( ml_data, ml_dc_er_log, l_config, file=filename)
}
#printf(">>> %f \n", proc.time()[3] - time0)

#v_times <- 0:20
#plot(v_times, seq(0,1,length.out=length(v_times)), type="n")
#sapply(2:num_repl, function(n) points(v_times, sim_prob_time(v_times, get_operation_n_latency_time(m_l_data$dc_er, n)), pch=n) )
#sapply(2:num_repl, function(n) points(v_times, sim_prob_time(v_times, get_operation_n_latency_time(m_l_data$dnc, n)), pch=n) )