full_input = readLines('input-data/05')

library(data.table)
midpoint = \(x) x[length(x)/2+1L]

# PART ONE

# Build keyed before|after table of rules
rules = full_input |>
  grep("[0-9]+[|][0-9]+", x=_, value=TRUE) |>
  tstrsplit("|", fixed=TRUE) |>
  setDT() |>
  setnames(c("before", "after"))
setkey(rules, before, after)

# build update sequences
updates = full_input |>
  grep(",", x=_, fixed=TRUE, value=TRUE) |>
  strsplit(",", fixed=TRUE)

get_violations = function(update) {
  # every pair of sequence entries
  # NB: combn will always return the earlier element first
  pairs = combn(update, 2L)
  # right join --> NA implies "no match" i.e. "no violation"
  as.integer(na.omit(rules[.(pairs[2L, ], pairs[1L, ]), which=TRUE]))
}
violation_idx = sapply(updates, get_violations)
is_right_order = lengths(violation_idx) == 0L
sum(as.integer(sapply(updates[is_right_order], midpoint)))

# PART TWO

swap_values = function(x, a, b) {
  tmp = x
  i = which(x == a)
  j = which(x == b)
  tmp[i] = x[j]
  tmp[j] = x[i]
  tmp
}
violations = rbindlist(lapply(violation_idx, \(i) rules[i]), idcol='update_idx')
setkey(violations, update_idx)
updates[!is_right_order]

sum(as.integer(sapply(which(!is_right_order), function(ii) {
  u = updates[[ii]]
  v = violations[.(ii)]
  # first pass of swapping not guaranteed to produce valid update, but
  #   repeatedly passing over & doing swap-based-fixes will get there eventually
  while (nrow(v)) {
    for (jj in seq_len(nrow(v))) u = swap_values(u, v$before[jj], v$after[jj])
    v = rules[get_violations(u)]
  }
  midpoint(u)
})))
