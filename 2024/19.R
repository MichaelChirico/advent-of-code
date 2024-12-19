library(data.table)
input = readLines('input-data/19')

patterns_full = sort(unlist(strsplit(input[1L], ",\\s+")))
patterns = setDT(tstrsplit(patterns_full, NULL))
patterns[, full := patterns_full]
patterns[, size := nchar(full)]
max_ptn = max(nchar(patterns_full))
setkeyv(patterns, paste0("V", seq_len(max_ptn)))
setindexv(patterns, lapply(seq_len(max_ptn), \(j) c(paste0("V", 1:j), "size")))

designs_full = tail(input, -2L)
design_chars = designs_full |>
  tstrsplit(NULL) |>
  setDT() |>
  as.matrix() |>
  unname()

can_split_at_any = function(chars) {
  # fail at char_i=j does _not_ imply failure at j+1,
  #   because we also join on 'size'. but we _could_
  #   cut the loop early once any valid split is found.
  for (char_i in seq_len(min(length(chars), max_ptn))) {
    if (can_split_at_char(chars, char_i)) return(TRUE)
  }
  return(FALSE)
}

can_split_at_char = function(chars, char_i) {
  before = as.list(head(chars, char_i))
  names(before) = paste0("V", seq_along(before))
  before = c(before, size = char_i)
  match_before = patterns[before, on=names(before), nomatch=NULL]
  if (!nrow(match_before)) return(FALSE)
  if (length(chars) == char_i) return(TRUE)
  # NB: inefficiently keeps trying, e.g. if 'bg' fails,
  #   we know 'bgg' will also fail. ignore for now...
  return(any(
    match_before[, by=.I, .(can = can_split_at_any(tail(chars, -size)))]$can
  ))
}

feasible = mapply(
  function(ii, nc) {
    if (ii %% 50L == 0L) cat(sprintf("design=%d\n", ii))
    can_split_at_any(design_chars[ii, seq_len(nc)])
  },
  seq_along(designs_full), nchar(designs_full)
)

sum(feasible)
