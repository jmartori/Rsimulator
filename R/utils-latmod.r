## utils-latmod



exp_snd <- function(t, l) {
  return (1 - exp(-l*t))
}

exp_rcv <- function(t, l) {
  return (1 - 0.5*exp(-l*t))
}


exp_snd_vis <- function(t, n, l) {
  return (exp_snd(t,l)^(n-1))
}

exp_rcv_vis <- function(t, n, l) {
  return ((exp(l*t)/n)*(1 - ( 1 - exp(-l*t))^n ))
}

par_snd <- function(t, scale, shape){
  if (scale < t){
    return (1 - (scale/t)^shape)
  }
  else{
    return(0)
  }
}

exp_causal <- function()
{
  
}

exp_fifo_ng <- function(t = 0, l)
{
  prod(exp_snd(t, l=l))
}