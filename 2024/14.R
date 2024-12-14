library(data.table)
library(nc)

show_snapshot = function(bots) {
  counts = matrix(0, size[1L], size[2L])
  for (ii in seq_len(nrow(bots))) {
    loc = as.matrix(bots[ii, .(curr_i, curr_j)])
    counts[loc] = counts[loc] + 1L
  }
  storage.mode(counts) = "character"
  counts[counts == "0"] = "."
  counts[row(counts) - 1L == (size[1L]-1L)/2L] = "x"
  counts[col(counts) - 1L == (size[2L]-1L)/2L] = "x"
  writeLines(apply(counts, 1L, paste, collapse=""))
  invisible()
}

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

# NB: written in j,i form to match the problem statement, hence rev()
size = rev(unlist(fread(input_file, nrows=1L, header=FALSE), use.names=FALSE))

# x %% n, except give 'n' not '0' for 'n %% n' and '0 %% n'
adj_mod = function(x, n) {
  fifelse(x %in% c(0L, n), n, x %% n)
}

# 0-based indexing strikes AGAIN
input[, `:=`(init_i=init_i + 1L, init_j=init_j + 1L)]
# initialize locations
input[, `:=`(curr_i=init_i, curr_j=init_j)]

for (sec in 1:100) {
  input[, `:=`(
    curr_i = adj_mod(curr_i + del_i, size[1L]),
    curr_j = adj_mod(curr_j + del_j, size[2L])
  )]
}

input[
  curr_i-1L != (size[1L]-1L)/2L
  & curr_j-1L != (size[2L]-1L)/2L,
  j = .N,
  by=.(
    top_half = curr_i < size[1L]/2,
    left_half = curr_j < size[2L]/2
  )
][, prod(N)]
