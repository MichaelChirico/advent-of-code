library(data.table)
library(nc)

input_file = 'input-data/14'

# https://github.com/tdhock/nc/issues/28
input = readLines(input_file) |>
  # NB: I augmented the input with the associated grid dimensions in the first line
  tail(-1L) |>
  # NB: 'i' and 'j' match the M[i,j] subscripting style
  capture_first_vec(list(
    "p=", init_j="[0-9]+",  as.integer, ",", init_i="[0-9]+", as.integer,
    " v=", del_j="[-0-9]+", as.integer,  ",", del_i="[-0-9]+", as.integer
  ))
# 0-based indexing strikes AGAIN
input[, `:=`(init_i=init_i + 1L, init_j=init_j + 1L)]

# NB: written in j,i form to match the problem statement, hence rev()
size = rev(unlist(fread(input_file, nrows=1L, header=FALSE), use.names=FALSE))
axes = (size - 1L)/2L + 1L

# debugging helper that turned out to be crucial
#   in part two.
show_snapshot = function(bots, show_axes=TRUE) {
  counts = matrix(0, size[1L], size[2L])
  for (ii in seq_len(nrow(bots))) {
    loc = as.matrix(bots[ii, .(curr_i, curr_j)])
    counts[loc] = counts[loc] + 1L
  }
  storage.mode(counts) = "character"
  counts[counts == "0"] = "."
  if (show_axes) {
    counts[row(counts) == axes[1L] | col(counts) == axes[2L]] = "x"
  }
  writeLines(apply(counts, 1L, paste, collapse=""))
  invisible()
}

# x %% n, except give 'n' not '0' for 'n %% n' and '0 %% n'
# NB: '%%' is already vectorized in both inputs!
adj_mod = function(x, n) {
  fifelse(x %in% c(0L, n), n, x %% n)
}

## PART ONE

# initialize locations
input[, `:=`(curr_i=init_i, curr_j=init_j)]
# iteratively update each bot's location
for (sec in 1:100) {
  input[, `:=`(
    curr_i = adj_mod(curr_i + del_i, size[1L]),
    curr_j = adj_mod(curr_j + del_j, size[2L])
  )]
}

input[
  curr_i != axes[1L] & curr_j != axes[2L],
  j = .N,
  # group by which side of each axis a coordinate is on
  by=.(
    top_half = curr_i < axes[1L],
    left_half = curr_j < axes[2L]
  )
][, prod(N)]

## PART TWO

sec = 0L
# reset
input[, `:=`(curr_i=init_i, curr_j=init_j)]
# idea: find a tree based on the assumption that when it appears,
#   there will be no overlapped robots.
repeat {
  sec = sec + 1L
  input[, `:=`(
    curr_i = adj_mod(curr_i + del_i, size[1L]),
    curr_j = adj_mod(curr_j + del_j, size[2L])
  )]
  if (!anyDuplicated(input, by=c("curr_i", "curr_j"))) break
}
show_snapshot(input, show_axes=FALSE) # yes: tree
cat(sprintf("Stopped after %d seconds\n", sec))
