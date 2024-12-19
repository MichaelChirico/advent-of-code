library(data.table)
input = readLines('input-data/19-test')

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
  # NB: ** DISTORTS THE INPUT ORDER **
  setkey() |>
  as.matrix() |>
  unname()

can_split_at = function(chars, char_i=NULL) {
  if (is.null(char_i)) {
    char_i = seq_along(chars)
  }
  if (length(char_i) > 1L) {
    return(any(sapply(char_i, can_split_at, chars=chars)))
  }
  if (char_i > max_ptn) return(FALSE)
  before = as.list(head(chars, char_i))
  names(before) = paste0("V", seq_along(before))
  before = c(before, size = char_i)
  match_before = patterns[before, on=names(before)]
  if (!nrow(match_before)) return(FALSE)
  if (length(chars) == char_i) return(TRUE)
  unmatched_chars = tail(chars, -char_i)
  # NB: inefficiently keeps trying, e.g. if 'bg' fails,
  #   we know 'bgg' will also fail. ignore for now...
  match_before[, by=.I, can_split_at(tail(chars, -size), NULL)]
  return(FALSE)
}

feasible = mapply(
  \(ii, nc) can_split_at(design_chars[ii, ], seq_len(nc)),
  seq_along(designs_full), nchar(designs_full)
)
