library(data.table)
input = readLines('input-data/16') |>
  strsplit(NULL) |>
  do.call(what = rbind)

draw_path = function(path, truncate=TRUE) {
  input[input == "."] = " "
  if (is.data.table(path)) {
    if ("path_id" %chin% names(path)) {
      path[, by=path_id, {
        cat(sprintf("path_id=%d\n", .BY$path_id))
        draw_path(.SD)
      }]
      return(invisible())
    }
    path = path[, cbind(row=row, col=col)]
  }
  if (nrow(path) > 1L) {
    input[path[1:(nrow(path)-1L),, drop=FALSE]] = "."
    input[path[nrow(path),, drop=FALSE]] = "X"
  }
  if (truncate) {
    draw_range = apply(path, 2L, range)
    draw_range[] = draw_range + c(-1L, 1L)
    draw = input[draw_range[1L]:draw_range[2L], draw_range[3L]:draw_range[4L]]
  } else {
    draw = input
  }
  apply(draw, 1L, paste, collapse="") |> c("") |> writeLines()
}

incremental_cost = function(prev_path, next_row) {
  if (nrow(prev_path) == 1L) {
    return(1L + 1000L * (prev_path$col == next_row$col))
  }
  latest3 = as.matrix(rbind(tail(prev_path[, .(row, col)], 2L), next_row[, .(row, col)]))
  is_turn = !all(diff(latest3[1:2, ]) == diff(latest3[2:3, ]))
  tail(prev_path$cum_cost, 1L) + 1L + 1000L * is_turn
}

# N/E/S/W steps
step_delta = cbind(
  row = c(0L, 1L, 0L, -1L),
  col = c(-1L, 0L, 1L, 0L)
)

next_paths = function(x, path_id) {
  last_step = c(tail(x$row, 1L), tail(x$col, 1L))
  if (all(last_step == end_idx)) {
    x = copy(x)[, sub_path_id := 0L]
    # since j= does not attempt to re-order columns
    setcolorder(x, c("sub_path_id", "row", "col", "cum_cost"))
    return(x)
  }
  rbindlist(idcol = 'sub_path_id', lapply(1:4, function(jj) {
    next_step = step_delta[jj,, drop=FALSE] + last_step
    if (input[next_step] == "#") return(NULL)
    next_row = data.table(next_step)
    # no loops within a path
    if (nrow(x[next_row, on=.NATURAL, nomatch=NULL])) return(NULL)
    next_row[, cum_cost := incremental_cost(x, .SD)]
    rbind(x, next_row)
  }))
}

remove_known_worse_paths = function(paths) {
  # Look ahead to the cost of the next step to account
  #   for any difference induced by a required turn that
  #   hasn't yet been taken on one path as of this step.
  paths_copy = copy(paths)
  paths_copy[, next_cum_cost := shift(cum_cost, -1L), by=path_id]
  last_two_steps = paths_copy[, tail(.SD, 2L), by=path_id]
  last_two_steps[, ord := seq_len(.N), by=path_id]
  path_comparison = merge(
    last_two_steps, paths_copy,
    by=c("row", "col"), suffixes=c("_new", "_prev")
  ) |>
    _[
      path_id_prev != path_id_new,
      if (.N == 2L) .SD,
      keyby=.(path_id_new, path_id_prev)
    ]
  if (!nrow(path_comparison)) return(paths)
  costlier_ids = path_comparison[
    # picking a "winner" from a tie requires more info
    ord == 1 & next_cum_cost_new != next_cum_cost_prev,
    unique(fifelse(
      next_cum_cost_new > next_cum_cost_prev,
      path_id_new,
      path_id_prev
    ))
  ]
  paths[!path_id %in% costlier_ids]
}

start_idx = which(input == "S", arr.ind=TRUE)
end_idx = which(input == "E", arr.ind=TRUE)

paths = data.table(start_idx)
paths[, `:=`(path_id = 1L, cum_cost = 0L)]
max_steps = 1L

start_end = rbind(data.table(start_idx), data.table(end_idx))
setkey(start_end)

total_steps = nrow(paths)
repeat {
  paths = paths |>
    _[, by=path_id, next_paths(.SD, .BY$path_id)] |>
    _[, path_id := .GRP, by=.(path_id, sub_path_id)] |>
    _[, sub_path_id := NULL] |>
    remove_known_worse_paths()

  if (nrow(paths) == total_steps) break
  total_steps = nrow(paths)
  max_steps = max_steps + 1L
  cat(sprintf(
    "After %d steps, exploring %d paths with %d total steps\n",
    max_steps, uniqueN(paths$path_id), nrow(paths)
  ))
}

paths[, by=path_id, cum_cost[.N]][, min(V1)]
