source("./tests/eq_causal_fifo.r")

mt <- cbind(seq(0,15,0.1), seq(0,15,0.1), seq(0,30,0.2), seq(0,75,0.5), seq(0,150,1))
vl <- c(1, 0.75, 0.5, 0.25, 0.1)
rounds <- 1

for (i in 1:rounds){
	for (i in 1:length(vl)){
		for (rate in c(1,5,10,25,50,100)){
			for(n in c(3, 5, 7, 11) ){ 
				a <- new.env()
				nb_causal_fifo(dg_env = a, v_times=mt[,i], l=vl[i], r=rate, n=n)		
		
				#save_filename <- sprintf("env_b_lambda-%.1f_rate-%d.rdata", vl[i], rate)
				#save(a, file=save_filename)
				d1 <- dist_compute_error(a$v, a$w)
				d2 <- dist_compute_error(a$v, a$w_eq)
				d3 <- dist_compute_error(a$v, a$vc)
				d4 <- rmse_compute_error(a$v, a$w)
				d5 <- rmse_compute_error(a$v, a$w_eq)
				d6 <- rmse_compute_error(a$v, a$vc)

				printf("%.2f\t%d\t%d\t%f\t%f\t%f\t%f\t%f\t%f\n", vl[i], rate, n, d1,d2,d3,d4,d5,d6)

				# Spring
				rm (a)
			}
		}	
	}
}