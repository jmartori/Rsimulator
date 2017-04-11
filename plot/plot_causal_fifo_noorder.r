source("tests/eq_causal_order.r")
source("tests/eq_fifo_order.r")
source("tests/eq_no_order.r")

height <- 8.27*2/3
width <- 11.69*2/3

# it stays with sim_* until we have the eq_no_order()
plot_causal_fifo_noorder <- function(v_times=seq(0,15,0.5), output_pdf=FALSE, filename_pdf="Rplot.pdf", ... ) {
  vn <- sim_no_order (t=v_times, ...)
  vf <- sim_fifo_order(t=v_times, ...)
  vc <- sim_causal_order(t=v_times, ..., bool=FALSE)
  
  m <- cbind(vn, vf, vc[[1]])
  if (output_pdf) pdf(file = filename_pdf)
    matplot(v_times, m, lty=1, type='l', xlab="Time", ylab="CDF")
    legend("right", c("No Order","FIFO Order", "Causal Order"), col=1:3, lty=c(1,1,1), bty = "n")
  if (output_pdf) gbg <- dev.off()
  
  return(m)
}

#v_times <- seq(0,15,0.5)
#vn <- apply(replicate(n = 10, sim_no_order (t=v_times, iter = 2000)), 1, mean)
#vf <- apply(replicate(n = 10, sim_fifo_order (t=v_times, iter = 2000)), 1, mean)
#vc <- apply(replicate(n = 10, sim_causal_order (t=v_times, iter = 2000, bool = F)[[1]]), 1, mean)
#
#m <- cbind(vn, vf, vc)
#pdf(file="plot_dnc_fifo_dc.pdf", height=height, width=width)
#  matplot(v_times, m, lty=1, type='l', xlab="Time", ylab="CDF")
#  legend("right", c("No Order","FIFO Order", "Causal Order"), col=1:3, lty=c(1,1,1), bty = "n")
#gbg <- dev.off()
