source("./R/utils-base.r")
source("./R/utils.r")
source("./R/utils-pbcl.r")


eq_rcv_msg_deliv <- function()
{
  stop("Function not yet Implemented")
}

simeq_rcv_msg_deliv <- function(m_data, deps=give_dependent_op(m_data$dc), t=0, r=1, l=0.1)
{
	repl_op <- apply(m_data$dc, 1, which.min)
	
	sapply(r, function(raux){
		r_op_dnc <- which(repl_op != raux)
		sapply( r_op_dnc, function(op) {
			r_dep_op <- max_n(which(repl_op[deps[[op]]] != raux ), n=50)
			
			return(1 - exp(-l * (t + (m_data$dnc[r_dep_op, raux] - m_data$dnc[op, raux]))) )
		}) 
	}) -> v_prob

	return (v_prob)
}

sim_rcv_msg_deliv <- function(m_data, v_times=seq(0,150,1), r=1:ncol(m_data$dc))
{
	#ptm1 <- proc.time()[3]

	repl_op <- apply(m_data$dc, 1, which.min)
	
	# It does the same as the following sapply but it takes 3500 times faster
	#data <- m_data$dc[r_op_dnc, r] - m_data$dnc[r_op_dnc, repl_op[r_op_dnc]]
	sapply(r, function(raux){
		r_op_dnc <- which(repl_op != raux)

		sapply(r_op_dnc, function(op) {
			m_data$dc[op, raux] - m_data$dnc[op, repl_op[op]]
		}) -> data
		return(sim_prob_time_2(v_times, data))
	}) -> v_prob
  
	#print(proc.time()[3] - ptm1)
	return (v_prob)
}

nb_rcv_msg_deliv <- function(m_data, ...)
{
  mw <- sim_rcv_msg_deliv(m_data, ...)
  
  sim_rcv_msg_deliv}