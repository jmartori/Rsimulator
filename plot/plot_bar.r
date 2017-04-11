#comparison with bar_plots
source("./R/utils.r")


par(mfrow=c(1,5))



config <- get_scenario(p_rpareto=0, rate_op=10)
m_data <- process_scenario(config, f_dc=matrix_causal_delivery_time_bt, f_dnc=matrix_delivery_time)

d_dc <- get_operation_delivery_time(m_data$dc)
d_dnc <- get_operation_delivery_time(m_data$dnc)
prob_dnc <- sim_prob_time(seq(0,100,1),d_dnc)
prob_dc <- sim_prob_time(seq(0,100,1),d_dc)

barplot(prob_dnc, horiz=TRUE, names.arg=v_times, col=rev(heat.colors(length(v_times))))
barplot(prob_dc, horiz=TRUE, names.arg=v_times, col=rev(heat.colors(length(v_times))))


config <- get_scenario(p_rpareto=0, rate_op=30)
m_data <- process_scenario(config, f_dc=matrix_causal_delivery_time_bt, f_dnc=matrix_delivery_time)

d_dc <- get_operation_delivery_time(m_data$dc)
prob_dc <- sim_prob_time(seq(0,100,1),d_dc)

barplot(prob_dc, horiz=TRUE, names.arg=v_times, col=rev(heat.colors(length(v_times))))


config <- get_scenario(p_rpareto=0, rate_op=50)
m_data <- process_scenario(config, f_dc=matrix_causal_delivery_time_bt, f_dnc=matrix_delivery_time)

d_dc <- get_operation_delivery_time(m_data$dc)
prob_dc <- sim_prob_time(seq(0,100,1),d_dc)

barplot(prob_dc, horiz=TRUE, names.arg=v_times, col=rev(heat.colors(length(v_times))))

config <- get_scenario(p_rpareto=0, rate_op=100)
m_data <- process_scenario(config, f_dc=matrix_causal_delivery_time_bt, f_dnc=matrix_delivery_time)

d_dc <- get_operation_delivery_time(m_data$dc)
prob_dc <- sim_prob_time(seq(0,100,1),d_dc)

barplot(prob_dc, horiz=TRUE, names.arg=v_times, col=rev(heat.colors(length(v_times))))
