## CAUSAL: Non Generalized vs Generalized Equations 
#
#
#####
source("tests/eq_causal_order.r")


height  <- 8.27 * 2/3
width <- 11.69 * 2/3 

v_times <- seq(0, 15, 0.1)

lambda <- 1
num_repl <- 5
rate_op <- 50
t <- v_times 

v.causal_ng  <- eq_causal_order_ng(n_op = 1000:1200, t = t, n=num_repl, r = rate_op, l = lambda)
#v.causal_ng_vis  <- eq_causal_order_ng_vis(n_op = 1000:1200, t = t, n=num_repl, r = rate_op, l = lambda)

# We need the vp_dist
r_data <- replicate(n=10, expr = process_scenario(get_scenario(rate_op = rate_op, len_lambda = lambda, num_repl = num_repl), 
                                                  f_dc=matrix_causal_delivery_time_bt, f_dnc=matrix_delivery_time)
                    )
apply(r_data, 2, function(mi){
  get_prob_deliv_remote_op_2(mi$dc, mi$dnc, num_repl = num_repl, v_times = v_times)
}) -> m_vp_dist
vp_dist <- apply(m_vp_dist, 1, mean)

sp.dc <- apply( r_data, 2, function(mi) sim_prob_time_2(v_times, get_operation_delivery_time(mi$dc)))
## easier like this than from sim_causal_order
l <- list(min=apply(sp.dc[,(1):(num_repl)], 1, min), max=apply(sp.dc[,(1):(num_repl)], 1, max), mean=apply(sp.dc[,(1):(num_repl)], 1, mean) )
v.causal_sim <- l$mean

v.causal_g   <- eq_causal_order_w_dist_nvis(vp_dist=vp_dist, t = v_times, r = rate_op, l = lambda, n = num_repl, kl_min = 0, kl_max = 25, kr_min = 1, kr_max = 150)
#v.causal_g_vis   <- eq_causal_order_w_dist(vp_dist=vp_dist, t = v_times, r = rate_op, l = lambda, n = num_repl)

#m <- cbind(v.causal_g, v.causal_ng, v.causal_sim, v.causal_ng_vis, v.causal_g_vis)

m <- cbind(v.causal_g, v.causal_ng, v.causal_sim)

#### plotting 

par(mfrow = c(1,1))

#pdf(file="plot_diff_ng-vs-g_causal.pdf", height=height, width=width)
  plot(v_times, m[,1] - m[,2], type = "l", lty = 1, xlab = "Time", ylab = "Prob.")
#gbg <- dev.off()

#pdf(file="plot_ng-vs-g_causal.pdf", height=height, width=width)
  matplot(v_times, cbind(m[,1], m[,2]), type = "l", lty = 1, xlab = "Time", ylab = "Prob")
  legend("right",  legend = c("Generalized", "Not Generalized"), col = 1:2 , lty = 1, lwd = 1, box.lwd = 0)
#gbg <- dev.off()

#pdf(file="plot_diff_ng-and-g-vs-sim_causal.pdf", height=height, width=width)
  matplot(v_times, cbind(m[,1] - m[,3], m[,2] - m[,3]), type = "l", lty = 1, xlab = "Time", ylab = "Prob.")
#gbg <- dev.off()

#pdf(file="plot_ng-and-g-vs-sim_causal.pdf", height=height, width=width)
  matplot(v_times, m, type = "l", lty = 1, xlab = "Time", ylab = "Prob")
  legend("right",  legend = c("Generalized", "Not Generalized", "Simulation"), col = 1:3 , lty = 1, lwd = 1, box.lwd = 0)
#gbg <- dev.off()


##### Go go go go ###