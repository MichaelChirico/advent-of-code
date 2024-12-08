input = readLines('input-data/08') |>
  strsplit(NULL) |>
  do.call(what = rbind)

freqs = input[input != "."] |>
  table() |>
  Filter(f = \(n) n>1L) |>
  names()

between = \(x, bounds) x >= bounds[1L] & x <= bounds[2L]
e <- environment()
upper_bounds = dim(input)

## PART ONE
is_antinode = matrix(FALSE, nrow(input), ncol(input))
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
sum(is_antinode)

## PART TWO
is_antinode = matrix(FALSE, nrow(input), ncol(input))
for (freq in freqs) {
  node_locs = which(input == freq, arr.ind=TRUE)
  combn(nrow(node_locs), 2L, simplify=FALSE, function(ij) {
    node_pair = node_locs[ij, ]
    delta = apply(node_pair, 2L, diff)
    all_antinodes = rbind(
      cbind(
        seq(to = node_pair[1L, "row"], by=delta[1L], length.out=upper_bounds[1L]),
        seq(to = node_pair[1L, "col"], by=delta[2L], length.out=upper_bounds[2L])
      ),
      cbind(
        seq(from = node_pair[2L, "row"], by=delta[1L], length.out=upper_bounds[1L]),
        seq(from = node_pair[2L, "col"], by=delta[2L], length.out=upper_bounds[2L])
      )
    )
    all_antinodes = all_antinodes[
      between(all_antinodes[, 1L], c(1L, upper_bounds[1L])) &
      between(all_antinodes[, 2L], c(1L, upper_bounds[2L])),,
      drop=FALSE
    ]
    e$is_antinode[all_antinodes] = TRUE
    NULL
  })
}
sum(is_antinode)
