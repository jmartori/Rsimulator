source("./R/utils.r")

#v_times <- seq(0, 120, 4)

#ptm0 <- unname(proc.time()[3])

#

replicate_process_scenario <- function (config, n=1, f_dc=NULL, f_dnc=NULL, f_fifo=NULL, f_dc_er=NULL) {
	lm_data <- list()

	for (i in 1:n){
		lm_data[[i]] <- process_scenario(config, f_dc, f_dnc, f_fifo, f_dc_er)
	}

	return (lm_data)
}

replicate_get_operation_delivery_time <- function(lm_data){
	n <- length(lm_data)
	d_dc <- NULL
	d_dnc <- NULL
	d_fifo <- NULL
	d_dc_er <- NULL

	if ( ! is.null(lm_data[[1]]$dc) ) {
		d_dc <- c(sapply(1:n, function(i) get_operation_delivery_time(lm_data[[i]]$dc)) )
	}
	if ( !is.null(lm_data[[1]]$dnc) ){
			d_dnc <- c(sapply(1:n, function(i) get_operation_delivery_time(lm_data[[i]]$dnc)))
	}
	if ( !is.null(lm_data[[1]]$fifo) ) {
		d_fifo <- c(sapply(1:n, function(i) get_operation_delivery_time(lm_data[[i]]$fifo)))
	}
	if ( !is.null(lm_data[[1]]$dc_er$data) ) {
			d_dc_er <- c(sapply(1:n, function(i) get_operation_delivery_time(lm_data[[i]]$dc_er$data)))
	}
	return (list(d_dc=d_dc, d_dnc=d_dnc, d_fifo=d_fifo, d_dc_er=d_dc_er))
}



config <- get_scenario(p_rpareto=0)
lm_data <- replicate_process_scenario(config, n=5, f_dc=matrix_causal_delivery_time_bt, f_dnc=matrix_delivery_time)
l_d <- replicate_get_operation_delivery_time(lm_data)
sim_dc <- sim_prob_time(v_times, l_d$d_dc)

print(sim_dc)
#
#
#printf(">>> Time Elapsed: %f, size_used = %d Bytes \n", unname(proc.time()[3]) - ptm0, object.size(lm_data))