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

## PART ONE
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

## PART TWO
designs_feasible = designs_full[feasible]
max_dsn = max(nchar(designs_feasible))
total_matches = 0L

possible_blocks = patterns_full[
  sapply(patterns_full, \(ptn) any(grepl(ptn, designs_feasible, fixed=TRUE)))
]
possible_patterns = data.table(pattern = possible_blocks, N = 1L)

repeat {
  possible_patterns[, exact_match := rowSums(outer(pattern, designs_feasible, `==`))]
  total_matches = total_matches + possible_patterns[, sum(exact_match * N)]
  # NB: no design is a substring of another design!
  possible_patterns = possible_patterns[exact_match == 0]
  possible_patterns = possible_patterns |>
    _[, .(
      pattern = c(outer(pattern, possible_blocks, paste0)),
      N = rep(N, each=length(possible_blocks))
    )] |>
    _[
      nchar(pattern) <= max_dsn,
      .(N = sum(N)),
      keyby=pattern
    ]
  # only calculate this once per pattern (while retaining duplicate rows if the same
  #   pattern can come from different sources)
  possible_patterns[, any_match := sapply(pattern, \(ptn) any(startsWith(designs_feasible, ptn)))]
  # idea: cache !any_match rows -- they are likely to come up again. e.g.
  #   if 'a|bcde' produced 'abcde' & found infeasible, we know 'ab|c|de'
  #   will also be infeasible --> no need to re-check. unsure how common.
  possible_patterns = possible_patterns[(any_match)]
  if (!nrow(possible_patterns)) break
  cat(sprintf(
    "%20.0f patterns under construction (%5d unique), size range %s. %15.0f matches found.\n",
    sum(possible_patterns$N), nrow(possible_patterns),
    toString(range(nchar(possible_patterns$pattern))),
    total_matches
  ))
}
