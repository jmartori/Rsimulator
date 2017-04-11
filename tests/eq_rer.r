source("tests/eq_fifo_order.r")
source("tests/eq_causal_order.r")

# r: rate_op
# n: #nodes
# l: Lambda
# t: v_times
# iter: #replicate
function_to_get_vp_dist <- function(r, n, t, l, iter = 10) {
  r_data <- replicate(n=iter, expr = process_scenario(get_scenario(rate_op = r, len_lambda = l, num_repl = n), 
                                                      f_dc=matrix_causal_delivery_time_bt, f_dnc=matrix_delivery_time)
                      )
  
  apply(r_data, 2, function(mi){
    get_prob_deliv_remote_op_2(mi$dc, mi$dnc, num_repl = n, v_times = seq(0,10,0.1))
  }) -> m_vp_dist
  vp_dist <- apply(m_vp_dist, 1, mean)
  return (vp_dist)
}

# This is FIFO
wrap_eq_fifo <- function(r, rate = 10, l = 1) eq_fifo_order(r = rate, t = r, l=l)

# This is total order
wrap_eq_total <- function(r, rate = 10, n = 5, l = 1) eq_causal_order(t = r, r = rate, n = n, l=l)

# Causal Simulation
wrap_sim_causal <- function(r, rate = 10, n = 5, l = 1) sim_causal_order(t = r, r = rate, n = n , bool = F, l=l)[[1]]

# Causal Simulation
wrap_eq_causal <- function(r, rate = 10, n = 5, l = 1, vp_dist = function_to_get_vp_dist(r=rate, n=n, t=r, l = l)) eq_causal_order_w_dist_nvis(vp_dist = vp_dist, r=rate, t=r, n=n, l=l)

# exp CDF
wrap_eq_exp <- function(r, l = 1)1-exp(-l*r)

f <- function(r, w, g = function(r){1-exp(-r)}, ...) {
  sapply(w, function(wi){
    sapply(r, function(ri){
      v <- 3/8 * exp(-wi-ri) * g(ri, ...)
    })
  }) -> m
  return(m)
}

f_all_four <- function(r = seq(0,10,0.1), w = seq(0,10,1), l = 2, rate = 10, n = 5){
  m.exp <- f(r = r, w = w, g=wrap_eq_exp, l = l)
  v.exp <- apply(m.exp, 1, max)
  
  m.fifo <- f(r = r, w = w, g=wrap_eq_fifo, l = l, rate = rate)
  v.fifo <- apply(m.fifo, 1, max)
  
  vp_dist <-  function_to_get_vp_dist(r=rate, n=n, t=0:10, l = l)
  m.causal <- f(r = r, w = w, g=wrap_eq_causal, vp_dist = vp_dist, rate = rate, n=n, l = l)
  v.causal <- apply(m.causal, 1, max)
  
  m.total <- f(r = r, w = w, g=wrap_eq_total, rate=rate, n = n, l = l)
  v.total <- apply(m.total, 1, max)
  
  return(cbind(v.exp, v.fifo, v.causal, v.total))
}

#### Setting up ####

### Example for one config at a time
#m <- f_all_four()
#matplot(seq(0,10,0.1), m, type = 'l', lty = 1, xlab = "Times", ylab = "Prob FP")
#legend("right", legend=c("No Order", "FIFO Order", "Causal Order", "Total Order"), lty = 1, bty = "n", col=1:4)

#pdf(file = "fp_numerical_analysis.pdf")
#  matplot(r, cbind(v.exp, v.fifo,v.causal, v.total), type = 'l', lty = 1, xlab = "Times", ylab = "Prob FP")
#  legend("right", legend=c("No Order", "FIFO Order", "Causal Order", "Total Order"), lty = 1, bty = "n", col=1:4)
#gbg <- dev.off()

#### Now the max to compare different lambdas

### Its confussing but its correct
## l^-1 = b
# a small b -> small FP
# a big b -> big FP

#v_l <- seq(0.001, 3, 3/150)
v_l <- seq(3, 0.001, -3/150)
v_l <- 1/v_l
#v_l <- 1:20

#v_l <- seq(1,100,5)
sapply(v_l, function (l){
  m <- f_all_four(l = l)
  p <- apply(m, 2, max)
  return(p)
})-> m.extend

pdf(file = "fp_numerical_analysis_diff_lambdas.pdf", height = 2/3 * 8.3, width = 2/3 * 11.7)
  m.rev <- apply(t(m.extend),2,function(line) rev(line))
  matplot(v_l[1:100], m.rev[1:100,], type = 'l', lty = 1, xlab = "Lambda", ylab = "Prob FP")
  legend("topright", legend=c("No Order", "FIFO Order", "Causal Order", "Total Order"), lty = 1, bty = "n", col=1:4)
gbg <- dev.off()
