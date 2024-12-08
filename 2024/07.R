input = 'input-data/07' |>
  readLines() |>
  strsplit(":?\\s+") |>
  lapply(as.numeric)

## PART ONE

ops = list(`*`, `+`)
total = 0
pb = txtProgressBar(max=length(input))
for (ii in seq_along(input)) {
  setTxtProgressBar(pb, ii)
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
close(pb)
cat(sprintf("%20.f\n", total))

## PART TWO

# +.01 needed for exact powers of 10
`%||%` = function(x, y) 10^ceiling(log10(y+.01))*x + y
ops = list(`*`, `+`, `%||%`)
total = 0
pb = txtProgressBar(max=length(input))
for (ii in seq_along(input)) {
  setTxtProgressBar(pb, ii)
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
close(pb)

cat(sprintf("%20.f\n", total))
