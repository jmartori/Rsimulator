## FIFO: Non Generalized vs Generalized Equations 
#
#
#####
source("tests/eq_fifo_order.r")

height  <- 8.27 * 2/3
width <- 11.69 * 2/3 

v_times <- seq(0, 15, 15/1000)

lambda <- 1
num_repl <- 5
rate_op <- 250
t <- v_times 

v.fifo_ng  <- eq_fifo_order_ng(n_op = 4001:5000, t = t, n=num_repl, r = rate_op, l = lambda)
v.fifo_sim <- sim_fifo_order(n = num_repl, t = t, r = rate_op/num_repl, l = lambda, iter = 10000)
v.fifo_g   <- eq_fifo_order(t = v_times, k = 1000, r = rate_op/num_repl, l = lambda)
m <- cbind(v.fifo_g, v.fifo_ng, v.fifo_sim)

#### plotting 

par(mfrow = c(1,1))

pdf(file="plot_diff_ng-vs-g_fifo.pdf", height=height, width=width)
  plot(v_times, m[,1] - m[,2], type = "l", lty = 1, xlab = "Time", ylab = "Prob.")
gbg <- dev.off()

pdf(file="plot_ng-vs-g_fifo.pdf", height=height, width=width)
  matplot(v_times, cbind(m[,1], m[,2]), type = "l", lty = 1, xlab = "Time", ylab = "Prob")
  legend("right",  legend = c("Generalized", "Not Generalized"), col = 1:2 , lty = 1, lwd = 1, box.lwd = 0)
gbg <- dev.off()


pdf(file="plot_diff_ng-and-g-vs-sim_fifo.pdf", height=height, width=width)
  matplot(v_times, cbind(m[,1] - m[,3], m[,2] - m[,3]), type = "l", lty = 1, xlab = "Time", ylab = "Prob.")
  legend("right",  legend = c("Generalized", "Not Generalized"), col = 1:2 , lty = 1, lwd = 1, box.lwd = 0)
gbg <- dev.off()

pdf(file="plot_ng-and-g-vs-sim_fifo.pdf", height=height, width=width)
  matplot(v_times, m, type = "l", lty = 1, xlab = "Time", ylab = "Prob")
  legend("right",  legend = c("Generalized", "Not Generalized","Simulation"), col = 1:3 , lty = 1, lwd = 1, box.lwd = 0)
gbg <- dev.off()

# example of the plot, but using smoothing functions to clean the bumpiness. for now we just use the original but...
#matplot(cbind(smooth.spline(m[,1] - m[,3], cv=T,spar = 0.4)$y, smooth.spline(m[,2] - m[,3], cv=T,spar = 0.4)$y), type = 'l', lty = 1)