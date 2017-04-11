# Given a rdata file 
# plots the fp figure

source("./R/utils.r")
source("./R/utils-pbcl.r")

k_pbs_config <- 4

filename <- sprintf("data/pbs_%d_c1.rdata", k_pbs_config)
pdf_filename <- sprintf("plot/pbs_%d_c1.pdf", k_pbs_config)

load(file=filename)

#v_times <- seq(1, 10000, 1)
#v_times <- seq(0.001, 10, 0.001)
v_times <- seq(0.1, 3000, 0.1)

n <- length(ml_data)



m_prob_dc_er <- matrix( c(sapply(1:n, function(k) sim_prob_time( v_times, get_operation_delivery_time(ml_data[[k]]$dc_er)))), ncol=n)
prob_dnc <- sim_prob_time( v_times, get_operation_delivery_time (ml_data[[1]]$dnc))
prob_dc <- sim_prob_time( v_times, get_operation_delivery_time (ml_data[[1]]$dc))

m <- matrix(c(prob_dnc, m_prob_dc_er, prob_dc ), ncol=(n+2))

pdf(pdf_filename)
	col <- 1:(n+2)
	#col <- c(1, 3:(n+2+1))

	matplot(v_times, m, type="l", lty=1, log="x", col=col, pch=1, xlab="Time", ylab="CDF")
	legend("bottomright", c("No Ordering", "10% FP", "1% FP", "0.1% FP", "Causal Ordering"), col=v_col[col], lwd = 1, lty=1, border="white")
	#legend("bottomright", c("No Ordering", "1% FP", "0.1% FP", "Causal Ordering"), col=col, lwd = 1, lty=1, border="white")
dev.off()