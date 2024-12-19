library(data.table)
input = fread('input-data/18', header=FALSE)
setnames(input, c('j', 'i'))
input[, names(.SD) := lapply(.SD, `+`, 1L)]

MAX = max(sapply(input, max))
# TRUE: '#', FALSE: '.'
walls = matrix(FALSE, MAX, MAX)

N_BYTES=1024
walls[input[1:N_BYTES, cbind(i, j)]] = TRUE

draw_path = function(path, truncate=TRUE) {
  path_grid = matrix(" ", MAX, MAX)
  path_grid[walls] = "#"
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
    path_grid[path[1:(nrow(path)-1L),, drop=FALSE]] = "."
    path_grid[path[nrow(path),, drop=FALSE]] = "X"
  }
  if (truncate) {
    draw_range = apply(path, 2L, range)
    draw = path_grid[draw_range[1L]:draw_range[2L], draw_range[3L]:draw_range[4L]]
  } else {
    draw = path_grid
  }
  apply(draw, 1L, paste, collapse="") |> c("") |> writeLines()
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
    if (any(next_step < 1L | next_step > MAX)) return(NULL)
    if (walls[next_step]) return(NULL)
    next_row = data.table(next_step)
    # no loops within a path
    if (nrow(x[next_row, on=.NATURAL, nomatch=NULL])) return(NULL)
    next_row[, cum_cost := tail(x$cum_cost, 1L) + 1L]
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
    ord == 1,
    unique(fifelse(
      next_cum_cost_new > next_cum_cost_prev,
      path_id_new,
      path_id_prev
    ))
  ]
  paths[!path_id %in% costlier_ids]
}

end_idx = cbind(row=MAX, col=MAX)
end_tbl = data.table(end_idx)

paths = data.table(row=1L, col=1L)
paths[, `:=`(path_id=1L, cum_cost=0L)]
max_steps = 1L

repeat {
  paths = paths |>
    _[, by=path_id, next_paths(.SD, .BY$path_id)] |>
    _[, path_id := .GRP, by=.(path_id, sub_path_id)] |>
    _[, sub_path_id := NULL] |>
    remove_known_worse_paths()

  if (nrow(DONE<-paths[end_tbl, on=.NATURAL, nomatch=NULL])) {
    cat(sprintf("DONE! Took %d steps.\n", paths[DONE,on='path_id', max(cum_cost)]))
    break
  }
  max_steps = max_steps + 1L
  cat(sprintf(
    "After %d steps, exploring %d paths with %d total steps\n",
    max_steps, uniqueN(paths$path_id), nrow(paths)
  ))
}
