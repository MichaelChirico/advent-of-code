input = readLines('input-data/13')

rules = split(input, cumsum(!nzchar(input)))

solve_rule = function(rule, offset=0) {
  rule = rule[nzchar(rule)]
  delta_x = as.integer(gsub(".*X[+]([0-9]+),.*", "\\1", rule[1:2]))
  delta_y = as.integer(gsub(".*Y[+]([0-9]+)$", "\\1", rule[1:2]))
  prize_xy = offset + as.integer(unlist(strsplit(gsub("[^0-9,]", "", rule[3L]), ",")))
  ans = round(solve(rbind(delta_x, delta_y), prize_xy), 3)
  if (isTRUE(all.equal(ans %% 1, c(0,0), tolerance=1e-3))) {
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

