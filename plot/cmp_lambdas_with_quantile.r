source("R/utils.r")


#l.02 <- run(k = 6, num_repl = 5, v_times = l.01$v_times)
#l.05 <- run(k = 7, num_repl = 5, v_times = l.01$v_times)
#l.1  <- run(k = 8, num_repl = 5, v_times = l.01$v_times)


run <- function(l=1, num_repl = 5, rate_op = 10, n = 50, window_op = 10000){
  r_data <- replicate(n=n, expr = process_scenario(get_scenario(num_repl = num_repl, rate_op = rate_op, len_lambda = l, window_op = window_op)
                                                   , f_dc=matrix_causal_delivery_time_bt, f_dnc=matrix_delivery_time))

  max_times <- round(max(apply( r_data, 2, function(mi) get_operation_delivery_time(mi$dc))))

  # 10k so the plot has a 10k hundred points, per line # The more points the better the plot is!
  dc <- apply( r_data, 2, function(mi) get_operation_delivery_time(mi$dc))
  dnc <- apply( r_data, 2, function(mi) get_operation_delivery_time(mi$dnc))
 
  dc <- as.numeric(dc)
  dnc <- as.numeric(dnc)
  
  return(list(dc = dc, dnc = dnc))
}

v_lambdas <- c(1, 0.5, 0.2, 0.1)
probs <- seq(0,1,0.01) #c(0.10, 0.25, 0.50, 0.75, 0.90, 0.99, 0.999)

sapply(v_lambdas, function (l) {
  ldata <- run(l = l)
  q.dc <- quantile(ldata$dc, probs = probs)
  q.dnc <- quantile(ldata$dnc, probs = probs)
  return (list(dc = q.dc,dnc = q.dnc, data = ldata))
}) -> qd


m.dc <- sapply(1:length(v_lambdas), function (i) qd[,i]$dc * v_lambdas[i])
m.dnc <- sapply(1:length(v_lambdas), function (i) qd[,i]$dnc * v_lambdas[i])

#pdf(file="plot_quantile_cmp_between_lambdas.pdf", height = 8.27*2/3, width = 11.69*2/3)
  matplot(cbind(m.dc[2:99,], m.dnc[2:99,]), type='l', lty = 1, xaxt = 'n', xlab="Quantile", ylab = "Time", col = c(1:4,5,5,5,5))
  axis(1, at = 1:98, labels=probs[2:99])
  legend("topleft", lty=1, col=1:5, legend=c("Lambda 1","Lambda 0.5","Lambda 0.2","Lambda 0.1","Baseline"))
#gbg <- dev.off()

