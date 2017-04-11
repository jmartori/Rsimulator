#number of iterations against the variability
# for fifo 
source("tests/eq_fifo_order.r")
v_times <- seq(0,15,0.1)

height <- 8.27*2/3
width <- 11.69*2/3


n_obs <- 10
v_n <- c(2, 5, 10, 25, 50, 100, 200, 500, 1000)

# From previous runs. You can uncomment the sapplies and generate a new one.
v <- c(0.0035631289,0.0019451940,0.0019703138,0.0011040467,0.0008224694,0.0005104499,0.0002525259,0.0002379000,0.0001410372)
#sapply(v_n, function(n) {
#  sapply(1:n_obs, function(i){
#    cat(sprintf("%d ", n))
#    m_w <- replicate(n, expr = sim_fifo_order(t = v_times, l = 1, r = 10))
#    
#    w.msd <- apply(m_w, 1, mean)
#    
#    return(w.msd)
#  }) -> val
#  apply(val, 1, sd) -> val
#  cat("\n")
#  return(val)
#}) -> m_sd
#v <- apply(m_sd, 2, mean)

par(mfrow=c(2,2))
  plot(v,type='l', lty = 1, xaxt = 'n',xlab = "#Iterations", ylab = "Normalized Standard Deviation", main = "v")
  axis(1,at = 1:9, labels = v_n)

  plot(v_n, v, type='l', lty = 1,xlab = "#Iterations", ylab = "Normalized Standard Deviation", main = "v_n v")
  
  plot(v_n, v/v_n, type='l', lty = 1, xlim = c(0,110),xlab = "#Iterations", ylab = "Normalized Standard Deviation", main = "v/v_n")
  
  plot(v_n, v*v_n, type='l', lty = 1, xlab = "#Iterations", ylab = "Normalized Standard Deviation", main = "v*v_n")
par(mfrow=c(1,1))


pdf ( file = "plot_number_iter_var_norm.pdf", height = height, width = width)
  plot(v_n, v/v_n, type='l', lty = 1, xlim = c(0,110),xlab = "#Iterations", ylab = "Normalized Standard Deviation")
gbg <- dev.off()
