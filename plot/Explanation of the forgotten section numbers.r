## Checking quantiles for the forgotten section

probs <- c(0.1, 0.25,0.5,0.75, 0.9, 0.99, 0.999)
v_lam <- c(0.1, 0.2, 0.5, 1)

sapply(v_lam, function(l){
  qexp( p = probs, rate = l) * l
}) -> dat

