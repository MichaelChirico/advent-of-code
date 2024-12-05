input = read.table('input-data/01')

## PART ONE
input |>
  lapply(sort) |>
  do.call(what = `-`) |>
  abs() |>
  sum()

## PART TWO
freq = as.data.frame(table(R = input[[2L]]))
sum(
  input[[1L]] * freq$Freq[match(input[[1L]], freq$R)],
  na.rm=TRUE
)
