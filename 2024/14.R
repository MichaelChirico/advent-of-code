library(data.table)
library(nc)

input = readLines('input-data/14') |>
  capture_first_vec(list(
    "p=", init_j="[0-9]+", ",", init_i="[0-9]+",
    " v=", del_j="[-0-9]+", ",", del_i="[-0-9]+"
  ))

status = matrix(nrow = 103L, ncol=101L)
