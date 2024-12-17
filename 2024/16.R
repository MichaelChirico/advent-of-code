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
  draw_range = apply(path, 2L, range)
  draw_range[] = draw_range + c(-1L, 1L)
  draw = input[draw_range[1L]:draw_range[2L], draw_range[3L]:draw_range[4L]]
  apply(draw, 1L, paste, collapse="") |> c("") |> writeLines()
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
all_tread_idx = matrix(nrow = sum(input %in% c(".", "S")), ncol=2L)
colnames(all_tread_idx) = c("row", "col")
all_tread_idx[prev_tread_idx<-1L, ] = start_idx
n_completed_paths = 0L
max_steps = 1L
run_env <- environment()
repeat {
  n_new_paths = 0L
  for (ii in seq_along(paths)) {
    path = paths[[ii]]
    last_step = path[nrow(path),, drop=FALSE]
    if (all(last_step == end_idx)) {
      paths[[ii]] = list(paths[[ii]])
      next
    }
    next_path = lapply(1:4, function(jj) {
      next_step = last_step + step_delta[jj,, drop=FALSE]
      if (input[next_step] != "#" && !step_touches(next_step, all_tread_idx[1:prev_tread_idx,, drop=FALSE])) {
        if (step_touches(next_step, end_idx)) {
          run_env$n_completed_paths = run_env$n_completed_paths + 1L
        } else {
          run_env$all_tread_idx[run_env$prev_tread_idx<-run_env$prev_tread_idx+1L, ] = next_step
        }
        rbind(path, next_step)
      }
    })
    cont_idx = lengths(next_path) > 0L
    n_new_paths = n_new_paths + sum(cont_idx)
    paths[[ii]] = next_path[cont_idx]
  }
  paths = unlist(paths, recursive=FALSE)
  if (!n_new_paths) break
  max_steps = max_steps + 1L
}

count_turns = function(path) {
  path = rbind(path[1L, ] - (0:1), path)
  turns = 0L
  for (ii in 3:nrow(path)) {
    if (!all(diff(path[ii - (1:0), ]) == diff(path[ii - (2:1), ]))) {
      turns = turns + 1L
    }
  }
  turns
}

min(sapply(paths, \(path) nrow(path) - 1L + 1000L * count_turns(path)))
