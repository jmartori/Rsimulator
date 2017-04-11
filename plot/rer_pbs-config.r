source("./R/utils.r")
source("load_pbs_config.r")

par(mfrow=c(2,3))
window_op <- 1000
max_iter <- 10

md.10.dc_er <- NULL
md.10.dnc <- NULL
md.10.dc <- NULL

md.1.dc_er <- NULL
md.1.dnc <- NULL
md.1.dc <- NULL

md.01.dc_er <- NULL
md.01.dnc <- NULL
md.01.dc <- NULL

	for (i in 2:4){
		for (iter in 1:max_iter) {
			config <- load_pbs_config_list(i)
			config_10 <- get_scenario(	num_repl=5, rate_op=10, window_op=window_op, len_lambda=config$lambdas, p_rpareto=config$ppareto, pareto_shape=config$shape, pareto_scale=config$scale, c1_dt=config$c1_10)
			config_1 <- get_scenario(	num_repl=5, rate_op=10, window_op=window_op, len_lambda=config$lambdas, p_rpareto=config$ppareto, pareto_shape=config$shape, pareto_scale=config$scale, c1_dt=config$c1_1)
			config_01 <- get_scenario(	num_repl=5, rate_op=10, window_op=window_op, len_lambda=config$lambdas, p_rpareto=config$ppareto, pareto_shape=config$shape, pareto_scale=config$scale, c1_dt=config$c1_01)
	
			m_data.10 <- process_scenario(config_10, f_dc=matrix_causal_delivery_time_bt, f_dc_er=matrix_causal_delivery_time_bt_with_error_recovery, f_dnc=matrix_delivery_time)
			m_data.1  <- process_scenario(config_01, f_dc=matrix_causal_delivery_time_bt, f_dc_er=matrix_causal_delivery_time_bt_with_error_recovery, f_dnc=matrix_delivery_time)
			m_data.01 <- process_scenario(config_01, f_dc=matrix_causal_delivery_time_bt, f_dc_er=matrix_causal_delivery_time_bt_with_error_recovery, f_dnc=matrix_delivery_time)
			
			md.10.dc_er <- rbind(md.10.dc_er, m_data.10$dc_er$data)
			md.10.dc <- rbind(md.10.dc, m_data.10$dc)
			md.10.dnc <- rbind(md.10.dnc, m_data.10$dnc)

			md.1.dc_er <- rbind(md.1.dc_er, m_data.1$dc_er$data)
			md.1.dc <- rbind(md.1.dc, m_data.1$dc)
			md.1.dnc <- rbind(md.1.dnc, m_data.1$dnc)

			md.01.dc_er <- rbind(md.01.dc_er, m_data.01$dc_er$data)
			md.01.dc <- rbind(md.01.dc, m_data.01$dc)
			md.01.dnc <- rbind(md.01.dnc, m_data.01$dnc)
		cat(".")
		}

		max_val <- max(c(	max(md.10.dc - md.10.dnc),
							max(md.10.dc_er - md.10.dnc),
							max(md.1.dc_er  - md.1.dnc),
							max(md.01.dc_er - md.01.dnc)))



		t <- seq(0, (round((max_val*1.1))+1), 0.1)

		
		m_sim_prob <- cbind(sim_prob_time(t, get_operation_delivery_time(md.10.dnc)),
							sim_prob_time(t, get_operation_delivery_time(md.10.dc_er)),
							sim_prob_time(t, get_operation_delivery_time(md.1.dc_er)),
							sim_prob_time(t, get_operation_delivery_time(md.01.dc_er)),
							sim_prob_time(t, get_operation_delivery_time(md.10.dc))
							)
		
		matplot(t, m_sim_prob, type="l", main=i, lty=1,log="x")
		print(i)
}

for (i in 2:4){
	config <- load_pbs_config_list(i)
	config_10 <- get_scenario(	num_repl=5, rate_op=10, window_op=window_op, len_lambda=config$fake_lambdas, p_rpareto=0, pareto_shape=config$shape, pareto_scale=config$scale, c1_dt=config$fake_c1_10)
	config_1 <- get_scenario(	num_repl=5, rate_op=10, window_op=window_op, len_lambda=config$fake_lambdas, p_rpareto=0, pareto_shape=config$shape, pareto_scale=config$scale, c1_dt=config$fake_c1_1)
	config_01 <- get_scenario(	num_repl=5, rate_op=10, window_op=window_op, len_lambda=config$fake_lambdas, p_rpareto=0, pareto_shape=config$shape, pareto_scale=config$scale, c1_dt=config$fake_c1_01)

	m_data.10 <- process_scenario(config_10, f_dc=matrix_causal_delivery_time_bt, f_dc_er=matrix_causal_delivery_time_bt_with_error_recovery, f_dnc=matrix_delivery_time)
	m_data.1  <- process_scenario(config_01, f_dc=matrix_causal_delivery_time_bt, f_dc_er=matrix_causal_delivery_time_bt_with_error_recovery, f_dnc=matrix_delivery_time)
	m_data.01 <- process_scenario(config_01, f_dc=matrix_causal_delivery_time_bt, f_dc_er=matrix_causal_delivery_time_bt_with_error_recovery, f_dnc=matrix_delivery_time)

	max_val <- max(c(	max(m_data.10$dc - m_data.10$dnc),
						max(m_data.10$dc_er$data - m_data.10$dnc),
						max(m_data.1$dc_er$data - m_data.1$dnc),
						max(m_data.01$dc_er$data - m_data.01$dnc)))



	t <- seq(0, (round((max_val*1.1))+1), 0.1)

	
	m_sim_prob <- cbind(sim_prob_time(t, get_operation_delivery_time(m_data.10$dnc)),
						sim_prob_time(t, get_operation_delivery_time(m_data.10$dc_er$data)),
						sim_prob_time(t, get_operation_delivery_time(m_data.1$dc_er$data)),
						sim_prob_time(t, get_operation_delivery_time(m_data.01$dc_er$data)),
						sim_prob_time(t, get_operation_delivery_time(m_data.10$dc))
						)
	
	matplot(t, m_sim_prob, type="l", main=sprintf("fake_%d", i), lty=1,log="x")
	print(i)
}