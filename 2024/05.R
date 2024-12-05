full_input = readLines('input-data/05')

library(data.table)
rules = full_input |>
  grep("[0-9]+[|][0-9]+", x=_, value=TRUE) |>
  tstrsplit("|", fixed=TRUE) |>
  setDT() |>
  setnames(c("before", "after"))
setkey(rules, before, after)

updates = full_input |>
  grep(",", x=_, fixed=TRUE, value=TRUE) |>
  strsplit(",", fixed=TRUE)

sum(as.integer(sapply(updates, function(u) {
  pairs = combn(u, 2L)
  if (all(is.na(rules[.(pairs[2L, ], pairs[1L, ]), which=TRUE]))) u[length(u)/2+1] else 0
})))
