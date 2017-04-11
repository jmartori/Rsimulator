# Test for fifo/causal quorum
source("./R/utils.r")

fn <- "po_quorum.rdata"

# First get multiple simulation data
rate_op <- 50
c1 <- 100
len_lambda <- 1
p_rpareto <- 0
window_op <- 1000

n <- 25

f_dc <- matrix_causal_delivery_time_bt
f_dnc <- matrix_delivery_time
f_fifo <- matrix_fifo_delivery_time
f_dc_er <- matrix_causal_delivery_time_bt_with_error_recovery

r_data <- replicate(n=n, expr = process_scenario(get_scenario(rate_op = rate_op, c1_dt = c1, len_lambda = len_lambda, p_rpareto = p_rpareto, window_op = window_op)
                                                 , f_dc=f_dc, f_dc_er=f_dc_er, f_dnc=f_dnc, f_fifo = f_fifo))

# combine all dc, dnc, and dc_er, fifo
lat.dc <-NULL
lat.dnc <- NULL
lat.fifo <- NULL
lat.dc_er <- NULL
for (i in 1:length(r_data[1,])){
  dat <- r_data[,i]
  lat.dc <- rbind(lat.dc, get_operation_latency_time(dat$dc))
  lat.dnc <- rbind(lat.dnc, get_operation_latency_time(dat$dnc))
  lat.fifo <- rbind(lat.fifo, get_operation_latency_time(dat$fifo))
  lat.dc_er <- rbind(lat.dc_er, get_operation_latency_time(dat$dc_er$data))
}
# Store lat's and r_data to file rdata
lat <- list(dc = lat.dc, dnc = lat.dnc, fifo = lat.fifo, dc_er = lat.dc_er)
save(file=fn, lat, r_data)
# And thats it for today.

# Check the density
# with get_operation_n_latency_time

# 
# Causal
#v_times <- seq(0,15,0.01)
#v2 <- sim_prob_time_2(v_times, get_operation_n_latency_time(lat.dc, n=2))
#v3 <- sim_prob_time_2(v_times, get_operation_n_latency_time(lat.dc, n=3))
#v4 <- sim_prob_time_2(v_times, get_operation_n_latency_time(lat.dc, n=4))
#v5 <- sim_prob_time_2(v_times, get_operation_n_latency_time(lat.dc, n=5))
#v <- cbind(v2,v3,v4,v5)

max_val <- max(sapply(1:length(r_data[1,]), function(i) max(r_data[,i]$dc)))
v_times <- seq(0, round(max_val)*1.2, 0.01)

# FiFo
v2 <- sim_prob_time_2(v_times, get_operation_n_latency_time(lat.fifo, n=2))
v3 <- sim_prob_time_2(v_times, get_operation_n_latency_time(lat.fifo, n=3))
v4 <- sim_prob_time_2(v_times, get_operation_n_latency_time(lat.fifo, n=4))
v5 <- sim_prob_time_2(v_times, get_operation_n_latency_time(lat.fifo, n=5))
v <- cbind(v2,v3,v4,v5)

eq_fifo <- eq_fifo_order(t=v_times, l=len_lambda, r=rate_op/5, k=200)

#matplot(v_times, v, lty=1,type="l", log="x")

f <- function(n=4, m=1:4, p) {
  a <- numeric(length = length(p))
  for (mi in m){
    a <- a + choose(n,mi) * p^mi * (1-p)^(n-mi)
  }
  return(a)
}
#matplot(v_times, cbind(v5, eq_fifo), lty=1, type="l", log="x")


# quorum eq fifo
p <- eq_fifo
dat <- cbind(f(4,1:4,p), f(4,2:4,p), f(4,3:4, p), f(4,4,p))

# v is from the simulation
# dat is from the equation
# and they nicely match, with a rmsd of 5.247535e-05
pdf(file="fifo_quorum_sim_eq.pdf", height = 8.27*2/3, width = 11.69*2/3)
  matplot(v_times, cbind(dat, v), lty = 1, type = "l", log = 'x', xlab = "Time", ylab = "CDF", col=c(rep.int(1,4), rep.int(2,4)))
  legend("left", legend=c("equation", "simulation", "2/5","3/5","4/5","5/5"), lty=c(1,1,0,0,0,0), col=c(1,2,1,1,1,1), pch = c(NA,NA,1:4))
  points(v_times[apply(dat, 2, function(col) which.closest(col, 0.5))], rep.int(0.5, ncol(dat)), pch = 1:4, lwd=3)
gbg <- dev.off()

pdf(file="fifo_quorum_error_sim_eq.pdf", height = 8.27*2/3, width = 11.69*2/3)
  matplot(v_times, cbind(dat-v), lty = 1, type="l", xlab = "Time", ylab="Prob Difference")
  legend("right", legend=c("2/5","3/5","4/5","5/5"), lty = 1, col=1:4)
gbg <- dev.off()
