source("R/utils-latmod.r")

v_times <- seq(0, 6, 0.1)

exp <- sapply(v_times, function(t) exp_snd(t = t, l = 1))
par <- sapply(v_times, function(t) par_snd(t = t, scale = 0.5, shape = 2))

pdf(file = "par-exp-dist.pdf", height = 8.27/2 , width = 11.69/2)
  matplot(v_times, cbind(exp, par), lty = 1, type="l", xlab = "Time", ylab = "CDF")
  legend("right", lty = 1, col = 1:2, legend = c("exponential", "pareto"))
gbg <- dev.off()
