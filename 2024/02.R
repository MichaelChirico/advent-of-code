input = lapply(strsplit(readLines('input-data/02'), " ", fixed=TRUE), as.integer)

is_safe = function(x) {
  d = diff(x)
  if (length(unique(sign(d))) != 1L) return(FALSE)
  ad = abs(d)
  all(ad >= 1 & ad <= 3)
}

## PART ONE

sum(sapply(input, is_safe))

## PART TWO

sum(sapply(input, \(x) {
  is_safe(x) ||
    any(sapply(seq_along(x), \(idx) is_safe(x[-idx])))
}))
