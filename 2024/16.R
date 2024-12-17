library(data.table)
input = readLines('input-data/16') |>
  strsplit(NULL) |>
  do.call(what = rbind)

draw_path = function(path) {
  input[input == "."] = " "
  if (nrow(path) > 1L) {
    input[path[1:(nrow(path)-1L),, drop=FALSE]] = "."
    input[path[nrow(path),, drop=FALSE]] = "X"
  }
  apply(input, 1L, paste, collapse="") |> writeLines()
}

start_idx = which(input == "S", arr.ind=TRUE)
end_idx = which(input == "E", arr.ind=TRUE)

# N/E/S/W steps
step_delta = cbind(
  c(0L, 1L, 0L, -1L),
  c(-1L, 0L, 1L, 0L)
)

step_touches = function(step, prev_steps) {
  any(step[, "row"] == prev_steps[, "row"] & step[, "col"] == prev_steps[, "col"])
}

paths = list(start_idx)
n_completed_paths=0L
repeat {
  n_new_paths = 0L
  for (ii in seq_along(paths)) {
    path = paths[[ii]]
    last_step = path[nrow(path),, drop=FALSE]
    if (all(last_step == end_idx)) {
      next
    }
    next_path = lapply(1:4, function(jj) {
      next_step = last_step + step_delta[jj,, drop=FALSE]
      if (input[next_step] != "#" && !step_touches(next_step, path)) {
        if (step_touches(next_step, end_idx)) n_completed_paths <<- n_completed_paths + 1L
        rbind(path, next_step)
      }
    })
    cont_idx = lengths(next_path) > 0L
    n_new_paths = n_new_paths + sum(cont_idx)
    paths[[ii]] = next_path[cont_idx]
  }
  if (!n_new_paths) break
  paths = unlist(paths, recursive=FALSE)
  if (any(sapply(paths, anyDuplicated))) stop("Some paths with duplicates")
  if (max(sapply(paths, nrow)) %% 50L == 0L) browser()
  cat(sprintf("%d new/continued paths, %d total, max(path_length)=%d, %d completed\n", n_new_paths, length(paths), max(sapply(paths, nrow)), n_completed_paths))
}
