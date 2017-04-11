### TEMP

# v is assumed sorted
get_breaks <- function(v, n_step=1)
{
  ret_val <- NULL
  len <- length(v)
  id <- 1
  while (id < len){
    tok <- which(v <= v[id]+n_step)
    id <- max(tok)
    if (length(tok) == 1) id <- id + 1
    ret_val <- c(ret_val, length(tok))
  }
  
  return(ret_val)
}

get_fp_by_breaks <- function(fp, breaks)
{
  ret_val <- NULL
  i_old <- 0
  for (i in breaks)
  {
    val <- mean(fp[(i_old+1):i])
    ret_val <- c(ret_val, val)
    
    i_old <- i
  }
  return(ret_val)
}

x <- sort(get_operation_delivery_time(ml_data[[1]]$dc_er), index.return=T)
breaks <- get_breaks(x$x, n_step = 0.5)
val <- get_fp_by_breaks(ml_dc_er_log[[1]]$fp[x$ix], breaks)
x <- ((0:(length(val)-1))*0.5)

plot(x,val, type="l")
