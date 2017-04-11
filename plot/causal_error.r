## Causal
source("tests/eq_causal_order.r")



f_compute <-matrix_causal_delivery_time_bt
name_error <- "test_causal_error_sim_eq.pdf"
name_plot <- "test_causal_sim_eq.pdf"

height  <- 8.27 * 2/3
width <- 11.69 * 2/3 

# n is the number of replications
rate_op <- 10
v_times <- seq(0,15,0.1)
n <- 2
num_repl <- 5

r_data <- replicate(n=n, expr = process_scenario(get_scenario(rate_op = rate_op, len_lambda = 1, num_repl = num_repl), f_dc=f_compute, f_dnc=matrix_delivery_time))

sp.dc <- apply( r_data, 2, function(mi) sim_prob_time_2(v_times, get_operation_delivery_time(mi$dc)))
sim_p.dc <- list(min=apply(sp.dc[,(1):(n)], 1, min), max=apply(sp.dc[,(1):(n)], 1, max), mean=apply(sp.dc[,(1):(n)], 1, mean) )


apply(r_data, 2, function(mi){
  get_prob_deliv_remote_op_2(mi$dc, mi$dnc, num_repl = num_repl, v_times = v_times)
}) -> m_vp_dist
vp_dist <- apply(m_vp_dist, 1, mean)

#v <- eq_causal_order_w_dist_nvis( vp_dist, t=v_times, r=rate_op, l = 1, n = num_repl)
v <- eq_causal_order_w_dist( vp_dist, t=v_times, r=rate_op/num_repl, l = 1, n = num_repl)

vw <- eq_causal_order_ng(4000:5000, t=seq(0,15,0.1), l=1, r=rate_op, n = num_repl)
w <- sim_p.dc$mean
pdf(file=name_error, height = height, width = width)
matplot(v_times,cbind(abs(v - w), abs(vw - w)), lty=1, type="l", xlab="Time", ylab="Prob Difference")
legend("right", legend=c("generalized eq - simulation", "non-generalized eq - simulation"), col=1:2, lty = 1)
gbg <- dev.off()

pdf(file=name_plot, height = height, width = width)
matplot(v_times, cbind(w, v, vw), type='l', lty = 1, xlab="Time", ylab="CDF")
legend("right", legend=c("simulation", "generalized eq", "non-generalized eq"), col=1:3, lty = 1)
gbg <- dev.off()