library(data.table)
input = as.numeric(strsplit(readLines('input-data/11'), " ")[[1L]])

apply_rule = function(x) {
  if (x == 0) return(1)
  nc = nchar(x) # NB: thankfully not affected by scipen
  if (nc %% 2L != 0L) return(2024*x)
  pow = 10**(nc/2L)
  c(x %/% pow, x %% pow)
}

# SOLUTION (just change 75<->25 between parts)

# freq gets too big for int32, so use numeric
#   (int64 would be better but yanno, R)
stones = data.table(value = input)[, .(freq=as.numeric(.N)), keyby=value]
for (ii in 1:75) {
  # ah, another place for fexplode()...
  #   https://github.com/Rdatatable/data.table/pull/4156
  # Instead of tracking individual stones, track buckets of stones & counts,
  #   otherwise the result requires 1.8PiB of storage. The key is to realize
  #   that most initial stones quickly become a power-of-2 number of digits
  #   which quickly shrinks to its individual digits, creating a lot of
  #   duplicates, e.g. after 7 blinks '0' becomes '4|0|4|8|20|24|4|0|4|8|8|0|9|6',
  #   i.e. 14 stones but only 7 unique.
  stones = stones[, {
    next_value = lapply(value, apply_rule)
    data.table(
      value = unlist(next_value),
      freq = rep(freq, lengths(next_value))
    )[, .(freq=sum(freq)), keyby=value]
  }]
}

sprintf("%30.0f", sum(stones$freq))
