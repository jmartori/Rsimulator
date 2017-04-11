


f <- function(probs, num_repl){

  sapply(probs, function(prob){
    1 - dbinom(x = 0, size = num_repl, prob = prob)
  }) -> ret_val
  return (ret_val)

}
