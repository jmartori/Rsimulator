## FIFO
source("tests/eq_fifo_order.r")

height  <- 8.27 * 2/3
width <- 11.69 * 2/3 
v_times <- seq(0,15,0.1)
rate_op <- 10

v <- eq_fifo_order(k = 24, t = v_times, l = 1, r = rate_op)
m_w <- replicate(100, expr = sim_fifo_order(t = v_times, l = 1, r = rate_op))

w.mean <- apply(m_w, 1, mean) 

#par(mfrow = c(1,2))
pdf(file="fifo_error_sim_eq.pdf", height = height, width = width)
  plot(v_times, w.mean - v, type="l", xlab = "Time", ylab = "Prob Difference")
gbg <- dev.off()

pdf(file="fifo_sim_eq.pdf", height = height, width = width)
  matplot(v_times, cbind(w.mean,v), lty = 1, type="l", ylab = "CDF", xlab = "Time")
  legend("right", legend=c("simulation", "equation"), col=1:2, lty = 1)
gbg <- dev.off()


## Causal
source("tests/eq_causal_order.r")

height  <- 8.27 * 2/3
width <- 11.69 * 2/3 

# n is the number of replications
rate_op <- 10
v_times <- seq(0,15,0.1)
n <- 100


r_data <- replicate(n=n, expr = process_scenario(get_scenario(rate_op = rate_op, len_lambda = 1, num_repl = 5), f_dc=matrix_causal_delivery_time_bt, f_dnc=matrix_delivery_time))

sp.dc <- apply( r_data, 2, function(mi) sim_prob_time_2(v_times, get_operation_delivery_time(mi$dc)))
sim_p.dc <- list(min=apply(sp.dc[,(1):(n)], 1, min), max=apply(sp.dc[,(1):(n)], 1, max), mean=apply(sp.dc[,(1):(n)], 1, mean) )


apply(r_data, 2, function(mi){
  get_prob_deliv_remote_op_2(mi$dc, mi$dnc, num_repl = 5, v_times = v_times)
}) -> m_vp_dist
vp_dist <- apply(m_vp_dist, 1, mean)

v <- eq_causal_order_w_dist(vp_dist, t=v_times, r=rate_op/5, l = 1, n = 5)

pdf(file="fifo_error_sim_eq.pdf", height = height, width = width)
  plot(v_times, v - sim_p.dc$mean, type="l", xlab="Time", ylab="Prob Difference")
gbg <- dev.off()

pdf(file="causal_sim_eq.pdf", height = height, width = width)
  matplot(v_times, cbind(sim_p.dc$mean, v), type='l', lty = 1, xlab="Time", ylab="CDF")
  legend("right", legend=c("simulation", "equation"), col=1:2, lty = 1)
gbg <- dev.off()

