input = as.numeric(strsplit(readLines('input-data/11'), " ")[[1L]])

stones = input

apply_rule = function(x) {
  if (x == 0) return(1)
  nc = nchar(x)
  if (nc %% 2L != 0L) return(2024*x)
  pow = 10**(nc/2L)
  c(x %/% pow, x %% pow)
}
apply_rules = function(x) {
  lapply(x, apply_rule)
}

## PART ONE
for (ii in 1:25) stones = unlist(apply_rules(stones))

## PART TWO

library(data.table)
stones = data.table(value = input)[, .(freq=as.numeric(.N)), keyby=value]
for (ii in 1:75) {
  stones = stones[, {
    next_value = apply_rules(value)
    data.table(
        value = unlist(next_value),
        freq = rep(freq, lengths(next_value))
    )[, .(freq=sum(freq)), keyby=value]
  }]
}
