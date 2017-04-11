source("./tests/eq_causal_fifo.r")

printf("Starting \n")

mt <- cbind(seq(0,15,0.1),seq(0,30,0.2), seq(0,150,1))
vl <- c(1, 0.5, 0.1)

for (i in 1:length(vl)){
	for (rate in c(10, 25, 100)){
		a <- new.env()
		nb_causal_fifo(dg_env = a, l=vl[i], r=rate, v_times=mt[,i])		

		save_filename <- sprintf("env_b_lambda-%.1f_rate-%d.rdata", vl[i], rate)
		save(a, file=save_filename)

		printf(">>> %s is ready.\n", save_filename)
		rm (a)
	}	
}