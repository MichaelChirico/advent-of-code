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
# M = cbind(rows, cols)
step_delta = cbind(c(0L, 1L, 0L, -1L), c(-1L, 0L, 1L, 0L))
next_steps = function(M) {
  M |>
    apply(MARGIN=1L, \(xy) sweep(step_delta, 2L, xy, `+`), simplify=FALSE) |>
    do.call(what = rbind) |>
    remove_out_of_bounds()
}

starts = data.table(which(input == 0L, arr.ind=TRUE))

starts[, by=.I, n_peaks := {
  paths = cbind(row, col)
  for (kk in 1:9) {
    if (!nrow(paths)) break
    possible_paths = next_steps(paths)
    keep = input[possible_paths] == kk
    paths = unique(possible_paths[keep, , drop=FALSE])
  }
  nrow(paths)
}]

starts[,sum(n_peaks)]
