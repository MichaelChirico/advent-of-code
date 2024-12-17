library(data.table)
input = readLines('input-data/16-test3') |>
  strsplit(NULL) |>
  do.call(what = rbind)

draw_path = function(path) {
  input[input == "."] = " "
  if (nrow(path) > 1L) {
    input[path[1:(nrow(path)-1L),, drop=FALSE]] = "."
    input[path[nrow(path),, drop=FALSE]] = "X"
  }
  draw_range = apply(path, 2L, range)
  draw_range[] = draw_range + c(-1L, 1L)
  draw = input[draw_range[1L]:draw_range[2L], draw_range[3L]:draw_range[4L]]
  apply(draw, 1L, paste, collapse="") |> c("") |> writeLines()
}

start_idx = which(input == "S", arr.ind=TRUE)
end_idx = which(input == "E", arr.ind=TRUE)

# N/E/S/W steps
step_delta = cbind(
  row = c(0L, 1L, 0L, -1L),
  col = c(-1L, 0L, 1L, 0L)
)

incremental_cost = function(prev_path, next_row) {
  if (nrow(prev_path) == 1L) {
    return(2L + 1000L * (prev_path$col == next_row$col))
  }
  latest3 = as.matrix(rbind(tail(prev_path[, .(row, col)], 2L), next_row[, .(row, col)]))
  is_turn = !all(diff(latest3[1:2, ]) == diff(latest3[2:3, ]))
  tail(prev_path$cum_cost, 1L) + 1L + 1000L * is_turn
}

next_paths = function(x, path_id) {
  last_step = c(tail(x$row, 1L), tail(x$col, 1L))
  if (all(last_step == end_idx)) {
    x = copy(x)[, sub_path_id := 0L]
    return(x)
  }
  rbindlist(idcol = 'sub_path_id', lapply(1:4, function(jj) {
    next_step = step_delta[jj,, drop=FALSE] + last_step
    if (input[next_step] == "#") return(NULL)
    next_row = data.table(next_step)
    next_row[, cum_cost := incremental_cost(x, .SD)]
    rbind(x, next_row)
  }))
}

paths = data.table(start_idx)
paths[, `:=`(path_id = 1L, cum_cost = 1L)]
max_steps = 1L

total_steps = nrow(paths)
repeat {
  n_new_paths = 0L
  paths = paths[, by=path_id, next_paths(.SD, .BY$path_id)]
  paths = paths[, path_id := .GRP, by=.(path_id, sub_path_id)]
  paths[, sub_path_id := NULL]
  if (anyDuplicated(paths, by=c("row", "col"))) browser()
  if (nrow(paths) == total_steps) break
  total_steps = nrow(paths)
  # for (path in unique(paths$path_id)) {
  #   path = paths[[ii]]
  #   last_step = c(row[.N], col[.N])
  #   if (!all(last_step == end_idx)) {
  #     paths[[ii]] = list(paths[[ii]])
  #     next
  #   }
  #   next_path = lapply(1:4, function(jj) {
  #     # don't allow loops within a path. earlier, I tried to prevent
  #     #   any path from re-treading where any other path had already
  #     #   been, but see 16-test3 -- because turns are so highly
  #     #   penalized, it's possible to find a path that reaches a point
  #     #   later but still "costs" less, because the first path to get
  #     #   there took more turns.
  #     if (input[next_step] != "#" && !step_touches(next_step, path)) {
  #       rbind(path, next_step)
  #     }
  #   })
  #   cont_idx = lengths(next_path) > 0L
  #   n_new_paths = n_new_paths + sum(cont_idx)
  #   paths[[ii]] = next_path[cont_idx]
  # }
  # paths = unlist(paths, recursive=FALSE)
  # if (!n_new_paths) break
  max_steps = max_steps + 1L
  cat(sprintf("%04d\n", max_steps))
}

min(sapply(paths, \(path) nrow(path) - 1L + 1000L * count_turns(path)))
