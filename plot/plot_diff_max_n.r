##############################################################
#
# We check the n_visibility (or partial visibility)
# of the messages that are send.
# 
##############################################################

source("./tests/utils-test.r")

source("./R/utils-base.r")
source("./R/utils-pbcl.r")
source("./R/utils.r")

plot(v_times, get_y_axis(v_times), type="n")
sapply(1:ncol(m_data$dc),function(i) points(v_times, sim_prob_time_2(v_times, get_odt_max_n(m_data$dc, n=i)), type="l", col=i))
