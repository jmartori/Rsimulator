

# Prob of a remote message being deliverable after t time
f <- function(m_data, v_times=seq(0,15,0.1), r=1) {
  dnc <- m_data$dnc[,r]
  dc <- m_data$dc[,r]
  #repl_op <- apply(dnc, 1, which.min)
  
  return (sim_prob_time_2(v_times, dc-dnc))
}

g <- function(m_data, v_times=seq(0,15,0.1), r=1) {
  dnc <- m_data$dnc[,r]
  dc <- m_data$dc[,r]
  repl_op <- apply(m_data$dnc, 1, which.min)
  m <- dc - dnc
  m <- m[which(repl_op != r)]
  
  return (sim_prob_time_2(v_times, m))
}



a <- sapply(1:5, function(r) g(m_data, v_times=seq(0,100,1), r=r))
matplot(a, lty=1, type="l")
