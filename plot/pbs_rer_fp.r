# make the pbs plots from the data/pbs_[1-4]_iter_25_... rdata files.

width = 0.60 * 11.69
height = 0.60 * 8.27


############################################
e_plt <- new.env()
fn <- "pbs_1_iter_25_c1_10-1-01_rate_50.pdf"
load("data/pbs_1_iter_25_c1_10-1-01_rate_50.rdata", envir = e_plt)
pdf (file = fn, width = width, height = height)
matplot(e_plt$v_times, cbind(e_plt$dnc$mean, e_plt$dc_er.c1$mean, e_plt$dc_er.c01$mean, e_plt$dc$mean), lty=1, type="l", log="x", xlab="Time", ylab="CDF", col=c(1,3:5))
legend("topleft", c("No Order", "FP 1 %", "FP 0.1 %", "Causal Order"), col=c(1,3:5), lty=1, bty = "n")
gbg <- dev.off()
rm (e_plt)
############################################
e_plt <- new.env()
fn <- "pbs_2_iter_25_c1_10-1-01_rate_50.pdf"
load("data/pbs_2_iter_25_c1_10-1-01_rate_50.rdata", envir = e_plt)
pdf (file = fn, width = width, height = height)
matplot(e_plt$v_times, cbind(e_plt$dnc$mean, e_plt$dc_er.c10$mean, e_plt$dc_er.c1$mean, e_plt$dc_er.c01$mean, e_plt$dc$mean), lty=1, type="l", log="x", xlab="Time", ylab="CDF")
legend("topleft", c("No Order", "Lambda 10 %", "FP 1 %", "FP 0.1 %", "Causal Order"), col=1:5, lty=1, bty = "n")
gbg <- dev.off()
rm (e_plt)
############################################
e_plt <- new.env()
fn <- "pbs_3_iter_25_c1_10-1-01_rate_50.pdf"
load("data/pbs_3_iter_25_c1_10-1-01_rate_50.rdata", envir = e_plt)
pdf (file = fn, width = width, height = height)
matplot(e_plt$v_times, cbind(e_plt$dnc$mean, e_plt$dc_er.c10$mean, e_plt$dc_er.c1$mean, e_plt$dc_er.c01$mean, e_plt$dc$mean), lty=1, type="l", log="x", xlab="Time", ylab="CDF")
legend("topleft", c("No Order", "Lambda 10 %", "FP 1 %", "FP 0.1 %", "Causal Order"), col=1:5, lty=1, bty = "n")
gbg <- dev.off()
rm (e_plt)
############################################
e_plt <- new.env()
fn <- "pbs_4_iter_25_c1_10-1-01_rate_50.pdf"
load("data/pbs_4_iter_25_c1_10-1-01_rate_50.rdata", envir = e_plt)
pdf (file = fn, width = width, height = height)
matplot(e_plt$v_times, cbind(e_plt$dnc$mean, e_plt$dc_er.c10$mean, e_plt$dc_er.c1$mean, e_plt$dc_er.c01$mean, e_plt$dc$mean), lty=1, type="l", log="x", xlab="Time", ylab="CDF")
legend("topleft", c("No Order", "Lambda 10 %", "FP 1 %", "FP 0.1 %", "Causal Order"), col=1:5, lty=1, bty = "n")
gbg <- dev.off()
rm (e_plt)