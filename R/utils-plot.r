source("./R/utils-base.r")

plotting <- function(m_data, filename, repl_op, t_op)
{

	# Plotting configuration 
	# Replica 1 gets color Red, Replica 2 Blue, ...
	colors <- c("red","blue","green","orange","yellow","purple","pink","brown")

	x_min <- 0
	x_max <- time_exp
	x <- x_min:x_max

	y_min <- 0
	y_max <- num_repl
	y <- seq(y_min,y_max, ((y_max - y_min) / (x_max - x_min)))

	# Open plotting device
	svg(filename=filename, width=14, height=7)

	# Plot Frame
	plot ( x, y, type = "n")

	# Replica Timelines
	for (i in 1:num_repl)
	{
		abline(h = i)
	}

	# Add segments of distribution
	for (i in 1:num_repl)
	{
		for (id in which(repl_op == i))
		{
			for (j in 1:num_repl)
			{
				if (j == i) next
				segments(m_data[id, i], i, m_data[id, j], j, lty=2, col=colors[i])
			}
		}
	}

	# Add points to the Timelines
	# This way the points is not covert by the other colors
	for (i in 1:num_repl)
	{
		repl_points <- get_points_by_repl (t_op, repl_op, i)
		num_points <- length (repl_points)	
		y_repl <- sample ((i-1):i, num_points, replace = TRUE,prob = c(0,1))
		points( repl_points, y_repl, type="p", pch = 19 )
	}

	garbage <- dev.off()
}


plotting_causality <- function(m_data, filename, repl_op, t_op)
{

	# Plotting configuration 
	# Replica 1 gets color Red, Replica 2 Blue, ...
	colors <- c("red","blue","green","orange","yellow","purple","pink","brown")

	x_min <- 0
	x_max <- time_exp
	x <- x_min:x_max

	y_min <- 0
	y_max <- length(m_data[1, ])
	y <- seq(y_min,y_max, ((y_max - y_min) / (x_max - x_min)))

	# Open plotting device
	svg(filename=filename, width=14, height=7)

	# Plot Frame
	plot ( x, y, type = "n")

	# Replica Timelines
	for (i in 1:y_max)
	{
		abline(h = i)
	}

	# Add segments of distribution
	for (i in 1:y_max)
	{
		for (id in which(repl_op == i))
		{
			for (j in 1:y_max)
			{
				if (j == i) next

				# Out of bounds exception... 
				#segments(m_data[id, i], i, m_data[id, j], j, lty=2, col=colors[i])
				segments(m_data[id, i], i, m_data[id, j], j, lty=2, col=colors[i])
			}
		}
	}

	# Add points to the Timelines
	# This way the points is not covert by the other colors
	for (i in 1:num_repl)
	{
		repl_points <- get_points_by_repl (t_op, repl_op, i)
		num_points <- length (repl_points)	
		y_repl <- sample ((i-1):i, num_points, replace = TRUE,prob = c(0,1))
		points( repl_points, y_repl, type="p", pch = 19 )
	}
	garbage <- dev.off()

}

#plotting_to_file <- function() 
#{
#	filename=sprintf('./plot/pbs-%d-%d%d%d_%.3f_%.3f_%.3f_%.3f_%d.png',session_id,num_replicas,num_write_repl,num_read_repl,l0_wreq,l1_wack,l2_rreq,l3_rres,iter)
#	png(filename=filename)
#	plot(results_plot[1:30], type="l")
#	garbage <- dev.off()	
#}