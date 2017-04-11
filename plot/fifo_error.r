## FIFO
source("tests/eq_fifo_order.r")

height  <- 8.27 * 2/3
width <- 11.69 * 2/3 
v_times <- seq(0,15,0.1)
rate_op <- 10
n <- 100

v <- eq_fifo_order(k = 24, t = v_times, l = 1, r = rate_op)
m_w <- replicate(n, expr = sim_fifo_order(t = v_times, l = 1, r = rate_op))

w.mean <- apply(m_w, 1, mean) 

pdf(file="fifo_error_sim_eq.pdf", height = height, width = width)
  plot(v_times, w.mean - v, type="l", xlab = "Time", ylab = "Prob Difference")
gbg <- dev.off()

pdf(file="fifo_sim_eq.pdf", height = height, width = width)
  matplot(v_times, cbind(w.mean,v), lty = 1, type="l", ylab = "CDF", xlab = "Time")
  legend("right", legend=c("simulation", "equation"), col=1:2, lty = 1)
gbg <- dev.off()




