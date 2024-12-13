input = readLines('input-data/13')

rule_id = cumsum(!nzchar(input))

split(input, rule_id) |>
  sapply(function(rule) {
    rule = rule[nzchar(rule)]
    delta_x = as.integer(gsub(".*X[+]([0-9]+),.*", "\\1", rule[1:2]))
    delta_y = as.integer(gsub(".*Y[+]([0-9]+)$", "\\1", rule[1:2]))
    prize_xy = as.integer(unlist(strsplit(gsub("[^0-9,]", "", rule[3L]), ",")))
    ans = round(solve(rbind(delta_x, delta_y), prize_xy), 3)
    if (isTRUE(all.equal(ans %% 1, c(0,0), tolerance=1e-3))) {
      sum(c(3, 1) * ans)
    } else {
      0
    }
  }) |>
  sum()
