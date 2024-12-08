input = readLines('input-data/08') |>
  strsplit(NULL) |>
  do.call(what = rbind)

freqs = input[input != "."] |>
  table() |>
  Filter(f = \(n) n>1L) |>
  names()

is_antinode = matrix(FALSE, nrow(input), ncol(input))
e <- environment()
upper_bounds = dim(input)

between = \(x, bounds) x >= bounds[1L] & x <= bounds[2L]
for (freq in freqs) {
  node_locs = which(input == freq, arr.ind=TRUE)
  combn(nrow(node_locs), 2L, simplify=FALSE, function(ij) {
    node_pair = node_locs[ij, ]
    delta = apply(node_pair, 2L, diff)
    antinode_pair = node_pair + rbind(-delta, delta)
    antinode_pair = antinode_pair[
      between(antinode_pair[, "row"], c(1L, upper_bounds[1L])) &
      between(antinode_pair[, "col"], c(1L, upper_bounds[2L])),,
      drop=FALSE
    ]
    e$is_antinode[antinode_pair] = TRUE
    NULL
  })
}
