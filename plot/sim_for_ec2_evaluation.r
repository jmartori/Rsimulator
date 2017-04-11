source("R/utils.r")
r_data <- replicate(n=1, expr = process_scenario(get_scenario(rate_op = 50, c1_dt=1, c2_dt =  1000, len_lambda = 0.014, p_rpareto = 0, window_op = 1000, num_repl = 5)
                                                     , f_dc=matrix_causal_delivery_time_bt, f_dc_er=matrix_causal_delivery_time_bt_with_error_recovery, f_dnc=matrix_delivery_time))


mean(r_data[,1]$dc_er$log$fp)

#max_times <- round(max(apply( r_data, 2, function(mi) get_operation_delivery_time(mi$dc))))
#v_times <- seq(0, max_times , max_times/10000)

#dc <- apply( r_data, 2, function(mi) get_operation_delivery_time(mi$dc))
#dnc <- apply( r_data, 2, function(mi) get_operation_delivery_time(mi$dnc))

#sp.dc  <- apply( r_data, 2, function(mi) sim_prob_time_2(v_times, get_operation_delivery_time(mi$dc)))
#sp.dnc <- apply( r_data, 2, function(mi) sim_prob_time_2(v_times, get_operation_delivery_time(mi$dnc)))

#dc <- apply (sp.dc, 1, mean)
#dnc <- apply (sp.dnc, 1, mean)

