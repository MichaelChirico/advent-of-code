input = 'input-data/07' |>
  readLines() |>
  strsplit(":?\\s+")

total = 0
for (ii in seq_along(input)) {
  lhs = input[[ii]][1L]

  rhs = input[[ii]][-1L]
  n_rhs = length(rhs)
  exprs = replicate(n_rhs-1L, c("*", "+"), simplify=FALSE) |>
    do.call(what = expand.grid) |>
    cbind(rbind(paste0(rhs, ")"))) # append ')' to get L-to-R grouping correctly
  # weave the operands into the right order
  ways_feasible = exprs[c(rbind(
    seq(n_rhs, length.out=n_rhs),
    c(seq_len(n_rhs-1L), 0L)
  ))] |>
    # construct the element-wise expressions like ((10)*19);
    #   evaluate whether it's "feasible" by wrapping in identical().
    #   NB 'isTRUE(all.equal())' _gives the wrong answer!_
    cbind(
      ..1 = paste0("identical(", lhs, ",", strrep("(", n_rhs)),
      ..2 = _,
      ..3 = ")"
    ) |>
    apply(MARGIN = 1L, paste, collapse = "") |>
    paste(collapse = "+") |>
    parse(text = _ ) |>
    eval()
  if (ways_feasible) total = total+as.numeric(lhs)
}
