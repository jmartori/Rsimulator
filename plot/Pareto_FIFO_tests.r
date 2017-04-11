f <- function(v_times, k, r, g, ... ) {
  sapply(v_times, function(t) {
    prod(sapply(0:k, function(ki){
      g(t + ki*r, ...)  
    }))
  })->ret_val
  return(ret_val)
}


v_times <- seq(0,3,0.001)
v_k <- 6

sapply(v_k, function(k){
  f(v_times, k = k, r = 0.1, g = exp_snd, l = 1)
})-> val_exp

sapply(v_k, function(k){
  f(v_times, k = k, r = 0.1, g = par_snd, scale = 0.235, shape = 10)
})-> val_par


matplot(v_times, val_par, lty=1, type='l', xlab = "Time", ylab = "CDF", col=1:11)
#matplot(v_times, val_exp,lty=1, type='l', xlab = "Time", ylab = "CDF", col=1:11)
legend("right", legend=v_k, col=1:11, lty=1)

# Sembla que els valors de k per pareto distributions son massa baixos... i sempre dona valors differents. Hem de mirar daplicar lo mateix k teniem del quantile
# Amb el pareto a veure si llavors funciona.
#
# Amb lo dels quantiles funciona per valors d k menors d 500. Que son tots menys un del PBS config ( el LNKD-HDD)

