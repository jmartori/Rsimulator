## Latency Models

height <- 8.27*2/3
width <- 11.69*2/3
log <- "x" # or ""
pos <- "left" # or "right"


source("replicas_latencies.r")
l.9 <- run(k = 5, num_repl = 5)
l.7 <- run(k = 6, num_repl = 5, v_times = l.9$v_times)
l.5 <- run(k = 7, num_repl = 5, v_times = l.9$v_times)
l.3 <- run(k = 8, num_repl = 5, v_times = l.9$v_times)

save(l.1,l.3,l.5,l.7,l.9, file = "dc_fifo_dnc_diff_lm.rdata")


#pdf(file=sprintf("plot_none_diff_lm_log.pdf", k), height=height, width=width)
	matplot(l.9$v_times, cbind(l.3$dnc$mean,l.5$dnc$mean,l.7$dnc$mean,l.9$dnc$mean), lty = 1, type="l", xlab="Time", ylab="CDF", log=log)
	legend(pos, legend=paste(c(0.1,0.2,0.5,1),"Lambda"), lty=1, col=4:1)
#gbg<-dev.off()

#pdf(file=sprintf("plot_fifo_diff_lm_log.pdf", k), height=height, width=width)
	matplot(l.9$v_times, cbind(l.3$fifo$mean,l.5$fifo$mean,l.7$fifo$mean,l.9$fifo$mean), lty = 1, type="l", xlab="Time", ylab="CDF", log=log)
	legend(pos, legend=paste(c(0.1,0.2,0.5,1),"Lambda"), lty=1, col=4:1)
#gbg<-dev.off()

#pdf(file=sprintf("plot_causal_diff_lm_log.pdf", k), height=height, width=width)
	matplot(l.9$v_times, cbind(l.3$dc$mean,l.5$dc$mean,l.7$dc$mean,l.9$dc$mean), lty = 1, type="l", xlab="Time", ylab="CDF", log=log)
	legend(pos, legend=paste(c(0.1,0.2,0.5,1),"Lambda"), lty=1, col=4:1)
#gbg<-dev.off()

#pdf(file=sprintf("plot_dnc_fifo_dc_diff_lm_log.pdf", k), height=height, width=width)
#	matplot(l.9$v_times, cbind(l.9$dnc$mean, l.9$fifo$mean, l.9$dc$mean), lty = 1, type="l", xlab="Time", ylab="CDF", log=log)
#	legend(pos, legend=c("no order","fifo order","causal order"), lty=1, col=1:3)
#gbg<-dev.off()


