source("./R/utils.r")

ptm0 <- proc.time()[3]

v_times <- seq(0, 100, 1)


rate_op <- 100
v_pad <- c(0,1000)
v_window_op <- c(1000,1000)

# No graph
#plot(v_times, seq(0, 1, 1/(length(v_times)-1) ), type="n")


iter <- 30
l_time <- list()

for (i in c(1,2))
{
	ptm1 <- unname(proc.time()[3])
		pad <- v_pad[i]
		window_op <- v_window_op[i]

		config <- get_scenario(rate_op=rate_op, p_rpareto=0, padding_op=pad, window_op=window_op)
		lm_data <- replicate_process_scenario(config, n=iter, f_dc=matrix_causal_delivery_time_bt, f_dnc=matrix_delivery_time)#, f_dc_er=matrix_causal_delivery_time_bt_with_error_recovery )
		
	ptm2 <- unname(proc.time()[3])
		pad <- v_pad[i]
		window_op <- v_window_op[i]*iter

		config <- get_scenario(rate_op=rate_op, p_rpareto=0, padding_op=pad, window_op=window_op)
		m_data <- process_scenario(config, f_dc=matrix_causal_delivery_time_bt, f_dnc=matrix_delivery_time)
			
		dc <- m_data$dc[(pad+1):(pad+window_op), ]
		d_dc <- get_operation_delivery_time(dc)
				
		sim_dc <- sim_prob_time(v_times, d_dc)
	#	points(v_times, sim_dc, col=i, pch=2)
	ptm3 <- unname(proc.time()[3])

	l_time[[i]] <- list(small=(ptm2-ptm1), big=(ptm3-ptm2))
}



printf(">>> %f\n", proc.time()[3] - ptm0)