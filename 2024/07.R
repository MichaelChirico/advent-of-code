input = 'input-data/07' |>
  readLines() |>
  strsplit(":?\\s+") |>
  lapply(as.numeric)

## PART ONE

ops = list(`*`, `+`)
total = 0
for (ii in seq_along(input)) {
  lhs = input[[ii]][1L]

  outputs = input[[ii]][2L]
  rhs = tail(input[[ii]], -2L)

  for (kk in rhs) {
    outputs = c(sapply(ops, \(f) f(outputs, kk)))
    outputs = outputs[outputs <= lhs]
    if (!length(outputs)) break
  }
  if (any(outputs == lhs)) total = total+lhs
}
cat(sprintf("%20.f\n", total))

## PART TWO

# +.01 needed for exact powers of 10
`%||%` = function(x, y) 10^ceiling(log10(y+.01))*x + y
ops = list(`*`, `+`, `%||%`)
total = 0
for (ii in seq_along(input)) {
  lhs = input[[ii]][1L]

  outputs = input[[ii]][2L]
  rhs = tail(input[[ii]], -2L)

  for (kk in rhs) {
    outputs = c(sapply(ops, \(f) f(outputs, kk)))
    # drop any that are already infeasible (since
    #   all ops are such that f(x,y)>=x) to save
    #   space & computation.
    outputs = outputs[outputs <= lhs]
    if (!length(outputs)) break
  }
  if (any(outputs == lhs)) total = total+lhs
}

cat(sprintf("%20.f\n", total))
