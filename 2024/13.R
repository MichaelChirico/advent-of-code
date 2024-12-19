input = readLines('input-data/13')

rules = split(input, cumsum(!nzchar(input)))

all_integer = \(x) isTRUE(all.equal(round(x, 3L) %% 1, c(0,0), tolerance=1e-3))

solve_rule = function(rule, offset=0) {
  rule = rule[nzchar(rule)]
  delta_x = as.integer(gsub(".*X[+]([0-9]+),.*", "\\1", rule[1:2]))
  delta_y = as.integer(gsub(".*Y[+]([0-9]+)$", "\\1", rule[1:2]))
  prize_xy = as.integer(unlist(strsplit(gsub("[^0-9,]", "", rule[3L]), ",")))
  ans = solve(rbind(delta_x, delta_y), offset + prize_xy)
  if (all_integer(ans)) {
    sum(c(3, 1) * ans)
  } else {
    0
  }
}

## PART ONE
rules |>
  sapply(solve_rule) |>
  sum() |>
  sprintf("%30.0f", ..1=_)

## PART TWO
rules |>
  sapply(solve_rule, offset=10000000000000) |>
  sum() |>
  sprintf("%30.0f", ..1=_)

