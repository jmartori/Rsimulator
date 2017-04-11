source("./R/utils-base.r")

parser_command_line_args <- function(argc, argv) {
	
	arguments = vector(length = 9)	
	if (argc == 0) arguments <- command_line_default_args()
	else {
		# Number of replicas
		# Default: 5
		arguments[1] <- argv[1]
	
		# Length of the experiment in seconds# 
		# Default: 100
		arguments[2] <- argv[2]
	
		# operations per second
		# Default: 0.1
		arguments[3] <- argv[3]
	
		# Rate of transmission between replicas (exp distribution)
		# Default: 0.1
		arguments[4] <- argv[4]
		
		# rate of rpareto vs rexp operations
		# Default: 0
		arguments[5] <- argv[5]
	
		# Pareto shape
		# Default: 0.235
		arguments[6] <- argv[6]
	
		# Pareto Scale
		# Default: 10
		arguments[7] <- argv[7]
	
	
		# "random" seed
		# Default: 42
		#if (argc > 7)
		#	arguments[8] <- gk_session_id
		#else  {
		#	arguments[8] <- "42"
		#}
		if (argc > 7)
			arguments[8] <- argv[8]
		else  {
			arguments[8] <- gk_session_id
		}
	
		# Plotting??
		# Default: FALSE	
		if (argc > 8)
			arguments[9] <- "TRUE"
		else  {
			arguments[9] <- "FALSE"
		}
	}

	return (arguments)
}

command_line_default_args <- function(){
	arguments = vector(length = 9)	

	arguments[1] <- 5
	arguments[2] <- 100
	arguments[3] <- 10
	arguments[4] <- 0.1
	arguments[5] <- 0.9122
	arguments[6] <- 0.235
	arguments[7] <- 10
	arguments[8] <- 42
	arguments[9] <- "FALSE"

	return (arguments)
}


am_i_a_script <- function()
{
	env_launch_settings <- commandArgs()

	if (env_launch_settings[1] == "littler"){
		return ("script")
	} else {
		if (env_launch_settings[1] == "/usr/lib/R/bin/exec/R"){
			if(length(env_launch_settings) == 1 ) return ("no_script")
			else return("script")
		} else {
			return("no_clue")
		}
	}
}