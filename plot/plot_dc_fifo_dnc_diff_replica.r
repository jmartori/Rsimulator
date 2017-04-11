## Replica
k <- 8
height <- 8.27*2/3
width <- 11.69*2/3
source("replicas_latencies.r")
#l.13 <- run(k = k, num_repl = 64)
#l.11 <- run(k = k, num_repl = 32, v_times = l.13$v_times)
#l.9 <- run(k = k, num_repl = 16, v_times = l.13$v_times)
#l.7 <- run(k = k, num_repl = 8, v_times = l.13$v_times)
#l.5 <- run(k = k, num_repl = 4, v_times = l.13$v_times)
#l.3 <- run(k = k, num_repl = 2, v_times = l.13$v_times)

#save(l.3,l.5,l.7,l.9,l.11,l.13, file = "dc_fifo_dnc_diff_repl.rdata")
load(file = "dc_fifo_dnc_diff_repl.rdata")

m.dc <- cbind(l.3$dc$mean,l.5$dc$mean,l.7$dc$mean,l.9$dc$mean, l.11$dc$mean, l.13$dc$mean)
m.dnc <- cbind(l.3$dnc$mean,l.5$dnc$mean,l.7$dnc$mean,l.9$dnc$mean, l.11$dnc$mean, l.13$dnc$mean)
m.fifo <- cbind(l.3$fifo$mean,l.5$fifo$mean,l.7$fifo$mean,l.9$fifo$mean, l.11$fifo$mean, l.13$fifo$mean)

v_cols <- 1:6
str_legend <- paste(c(2**(1:6)),"#replicas")

pdf(file=sprintf("plot_none_diff_numrepl.pdf", k), height=height, width=width)
	matplot(l.13$v_times, m.dnc, lty = 1, type="l", xlab="Time", ylab="CDF", col=v_cols)
	legend("right", legend=str_legend, lty=1, col=v_cols)
gbg<-dev.off()

pdf(file=sprintf("plot_fifo_diff_numrepl.pdf", k), height=height, width=width)
	matplot(l.13$v_times, m.fifo, lty = 1, type="l", xlab="Time", ylab="CDF", col=v_cols)
	legend("right", legend=str_legend, lty=1, col=v_cols)
gbg<-dev.off()

pdf(file=sprintf("plot_causal_diff_numrepl.pdf", k), height=height, width=width)
	matplot(l.13$v_times, m.dc, lty = 1, type="l", xlab="Time", ylab="CDF", col=v_cols)
	legend("right", legend=str_legend, lty=1, col=v_cols)
gbg<-dev.off()
	
pdf(file=sprintf("plot_causal_and_fifo_diff_numrepl.pdf", k), height=height, width=width)
	matplot(l.13$v_times, cbind(m.dc, m.fifo), lty = c(rep.int(1,6),rep.int(2,6)), col = v_cols, type="l", xlab="Time", ylab="CDF")
	legend("right", legend=str_legend, lty=1, col=v_cols)
	legend("bottomright", legend=c("FIFO","Causal"), lty=2:1, col=1)
gbg<-dev.off()

#pdf(file=sprintf("plot_dnc_fifo_dc_diff_numrepl.pdf", k), height=height, width=width)
#	matplot(l.9$v_times, cbind(l.5$dnc$mean,l.5$fifo$mean,l.5$dc$mean), lty = 1, type="l", xlab="Time", ylab="CDF")
#	legend("right", legend=c("no order","fifo order","causal order"), lty=1, col=1:3)
#gbg<-dev.off()


