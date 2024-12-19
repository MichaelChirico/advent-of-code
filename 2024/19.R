library(data.table)
input = readLines('input-data/19-test')

patterns_full = sort(unlist(strsplit(input[1L], ",\\s+")))
patterns = setDT(tstrsplit(patterns_full, NULL))
setkey(patterns)

designs_full = tail(input, -2L)
design_chars = designs_full |>
  tstrsplit(NULL) |>
  setDT() |>
  # NB: ** DISTORTS THE INPUT ORDER **
  setkey() |>
  as.matrix() |>
  unname()

can_split_at = function(chars, char_i) {
  if (char_i > ncol(patterns)) return(0L)
  before = as.list(head(chars, char_i))
  can_before = length(patterns[before, which=TRUE]) > 0L
  if (!can_before) return(-1L)
  if (length(chars) == char_i) return(1L)
  n_after = 1L
  repeat {
    val = can_split_at(tail(chars, -char_i), n_after)
    if (val == 1L) return(TRUE)
    if (val == -1L) break
    n_after = n_after + 1L
  }
  return(0L)
}

feasible = sapply(seq_along(designs_full), function(ii) {
  any(sapply(
    seq_len(nchar(designs_full)[ii]),
    \(char_i) can_split_at(design_chars[ii, ], char_i)
  ))
})
  char_i = 1L
  pattern_subset = patterns[design_chars[ii, char_i], which=TRUE]
}
