# List Structures from the pbs paper.
# each position from 1 to 4 is a different configuration

# The *c1_* values come from the search_c1_for_fp.r script. The configuration has to be changed so it gives the right values for each parameter.
load_pbs_config_list <- function(n=1){
	pbs_config <- list (ppareto=c(0.9122, 0.38, 0.939, 0.982, 0,0,0,0),
						scale=c(0.235,1.05,3,1.5,0,0,0,0),
						shape=c(10,1.51,3.35,3.8,0,0,0,0),
						lambdas=c(1.66, 0.183, 0.0028, 0.0217,0.1,0.2,0.5,1),
						fake_lambdas=c(36.02, 0.3377, 0.9080, 2.2440, 0.1,0.3,0.5,1),
						c1_10=c(0, 4.75, 1.125, 0.39,     NA,NA,NA,NA),
						c1_1= c(0.039, 17.61, 5.046, 1.97,NA,NA,NA,NA),
						c1_01=c(0.817, 32.56, 12.23, 4.46,NA,NA,NA,NA),
						fake_c1_10=c(NA, 4.000, 1.31, 0.37,NA,NA,NA,NA),
						fake_c1_1= c(0.039, 11.25, 3.76, 1.44,NA,NA,NA,NA),
						fake_c1_01=c(0.117, 17.73, 6.24, 2.52,NA,NA,NA,NA)
						)

	return (lapply(pbs_config, '[[', n))
}



# Make the function that gives the fake_lambda
#	config <- get_scenario(len_lambda=len_lambda, p_rpareto=p_rpareto, pareto_shape=pareto_shape, pareto_scale=pareto_scale) 
#
#
#	v <- get_transmission_operation_times(num_op, config)
#	l <- ln(2)/median(v)
#
#	print(l)


