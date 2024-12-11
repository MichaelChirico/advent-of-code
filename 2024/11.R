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
  unlist(lapply(x, apply_rule))
}

for (ii in 1:25) stones = apply_rules(stones)
