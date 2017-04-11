source("tests/eq_fifo_order.r")
f <- function(v_times, k, r, g, ... ) {
  sapply(v_times, function(t) {
    prod(sapply(0:k, function(ki){
      g(t + ki*r, ...)  
    }))
  })->ret_val
  return(ret_val)
}

n <- 25
r <- 1/10 # 10 is the rate of operations

v_times <- seq(0, 3, 0.001)
k <- 6
pareto_scale <- 0.235
pareto_shape <- 10

v_times <- seq(0, 200, 0.1)
k <- 357
pareto_scale <- 3
pareto_shape <- 3.35

v_times <- seq(0, 50, 0.1)
k <- 134
pareto_scale <- 1.5
pareto_shape <- 3.8

val <- f(v_times, k = k, r = r, g = par_snd, scale = pareto_scale, shape = pareto_shape)
replicate(100,
{
  msp <- replicate(n,sim_fifo_order(t=v_times , p_rpareto = 1, pareto_scale = pareto_scale, pareto_shape=pareto_shape))
  msp.mean <- apply(msp, 1, mean)
  
  mean(abs(val - msp.mean))
}) -> v
#matplot(v_times, cbind(val, msp.mean),lty=1, type='l', log='x')

mean(abs(val - msp.mean))