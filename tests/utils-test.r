rmse_compute_error <- function(v,w)
{
	return(sum(abs(v^2 - w^2))/length(v))
}

dist_compute_error <- function(v,w)
{
	return(sum(abs((v - w)^2))/length(v))
}


get_y_axis <- function(v_x, y_min=0, y_max=1)
{
	v_y <- seq(y_min, y_max, (y_max-y_min)/length(v_x))
	len_y <- length(v_y)
	len_x <- length(v_x)
	
	if (len_y < len_x){
		v_y <- c(v_y, max(v_y))
	} 
	if (len_y > len_x) {
		v_y <- v_y[-which.max(v_y)]
	}
	return (v_y)
}

f_err <- function( v_l=1, d=0,iter=2500, v_t)
{
	v_err <- NULL
	for (l in v_l) {
		v <- eq_rcv_after_3(t=v_t, l=l)
		w <- sim_rcv_after_3(t=v_t, l=l, iter=iter)
		err <- rmse_compute_error(v,w)
		v_err <- c(v_err, err)
	}
	return(v_err)
}
