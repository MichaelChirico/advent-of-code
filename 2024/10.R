library(data.table)
input = readLines('input-data/10') |>
  strsplit(NULL) |>
  lapply(as.integer) |>
  do.call(what = rbind)
map_dim = dim(input)

# don't worry about drop: always >=2 valid steps
remove_out_of_bounds = function(M, bounds = map_dim) {
  M[M[,1L] > 0L & M[,1L] <= bounds[1L] & M[,2L] > 0L & M[,2L] <= bounds[2L], ]
}
# N/E/S/W steps
step_delta = cbind(
  c(0L, 1L, 0L, -1L),
  c(-1L, 0L, 1L, 0L)
)
# matrix of current positions along a trail |-> matrix of valid next positions
# M = cbind(rows, cols)
next_steps = function(M) {
  M |>
    apply(MARGIN=1L, \(xy) sweep(step_delta, 2L, xy, `+`), simplify=FALSE) |>
    do.call(what = rbind) |>
    remove_out_of_bounds()
}
valid_next_steps = function(possible_steps, target_value) {
  keep = input[possible_steps] == target_value
  possible_steps[keep, , drop=FALSE]
}

starts = data.table(which(input == 0L, arr.ind=TRUE))

## PART ONE
starts[, by=.I, n_distinct_peaks := {
  paths = cbind(row, col)
  for (kk in 1:9) {
    if (!nrow(paths)) break
    paths = paths |>
      next_steps() |>
      valid_next_steps(kk) |>
      unique()
  }
  nrow(paths)
}]

sum(starts$n_distinct_peaks)

## PART TWO
starts[, by=.I, n_paths := {
  paths = cbind(row, col)
  for (kk in 1:9) {
    if (!nrow(paths)) break
    paths = paths |>
      next_steps() |>
      valid_next_steps(kk)
  }
  nrow(paths)
}]

sum(starts$n_paths)
