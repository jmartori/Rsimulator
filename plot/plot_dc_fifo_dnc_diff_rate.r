## Rate
k <- 8
height <- 8.27*2/3
width <- 11.69*2/3
source("replicas_latencies.r")
## uncomment accordingly to generate new rdata files
#l.1 <- run(k = k, rate_op = 1)
#l.3 <- run(k = k, rate_op = 10, v_times = l.1$v_times)
#l.5 <- run(k = k, rate_op = 25, v_times = l.1$v_times)
#l.7 <- run(k = k, rate_op = 50, v_times = l.1$v_times)
#l.9 <- run(k = k, rate_op = 100, v_times = l.1$v_times)
#l.11 <- run(k = k, rate_op = 200, v_times = l.1$v_times)
#l.13 <- run(k = k, rate_op = 1000, v_times = l.1$v_times)

#save(l.1,l.3,l.5,l.7,l.9,l.11,l.13, file = "ffu_dc_fifo_dnc_diff_rate.rdata")

load(file = "plot/dc_fifo_dnc_diff_rate.rdata")

m.dc <- cbind(l.1$dc$mean, l.3$dc$mean,l.5$dc$mean,l.7$dc$mean,l.9$dc$mean, l.11$dc$mean, l.13$dc$mean)
m.dnc <- cbind(l.1$dnc$mean, l.3$dnc$mean,l.5$dnc$mean,l.7$dnc$mean,l.9$dnc$mean)
m.fifo <- cbind(l.1$fifo$mean, l.3$fifo$mean,l.5$fifo$mean,l.7$fifo$mean,l.9$fifo$mean, l.11$fifo$mean, l.13$fifo$mean)

str_legend <- paste(c(1,10,25,50,100, 200, 1000),"rate ops")
v_cols <- c(1:6, 8)

pdf(file=sprintf("plot_none_diff_rate.pdf", k), height=height, width=width)
	matplot(l.9$v_times, m.dnc, lty = 1, type="l", xlab="Time", ylab="CDF")
	legend("right", legend=str_legend , lty=1, col=v_cols)
gbg<-dev.off()

pdf(file=sprintf("plot_fifo_diff_rate.pdf", k), height=height, width=width)
	matplot(l.9$v_times, m.fifo, lty = 1, type="l", xlab="Time", ylab="CDF")
	legend("right", legend=str_legend, lty=1, col=v_cols)
gbg<-dev.off()

pdf(file=sprintf("plot_causal_diff_rate.pdf", k), height=height, width=width)
	matplot(l.9$v_times, m.dc, lty = 1, type="l", xlab="Time", ylab="CDF")
	legend("right", legend=str_legend, lty=1, col=v_cols)
gbg<-dev.off()
	
pdf(file=sprintf("plot_causal_and_fifo_diff_rate.pdf", k), height=height, width=width)
	matplot(l.9$v_times, cbind(m.dc, m.fifo), lty = c(rep.int(1,7),rep.int(2,7)), col=v_cols, type="l", xlab="Time", ylab="CDF")
	legend("right", legend=str_legend, lty=1, col=v_cols)
	legend("bottomright", legend=c("FIFO","Causal"), lty=2:1, col=1)
gbg<-dev.off()

#pdf(file=sprintf("plot_dnc_fifo_dc_diff_rate.pdf", k), height=height, width=width)
#	matplot(l.9$v_times, cbind(l.5$dnc$mean,l.5$fifo$mean,l.5$dc$mean), lty = 1, type="l", xlab="Time", ylab="CDF")
#	legend("right", legend=c("no order","fifo order","causal order"), lty=1, col=1:3)
#gbg<-dev.off()