input = readLines('input-data/15')

mat_collapse = \(x) apply(x, 1L, paste, collapse="")
show_snapshot = function(before, after = NULL, attempted_rule = NULL) {
  if (!is.null(after)) {
    if (isTRUE(all.equal(before, after))) {
      all_lines = c(mat_collapse(after), "(cannot move)")
    } else {
      un_step = -movements[[attempted_rule]]
      arr = switch(attempted_rule, `>` = "→", `^` = "↑", `<` = "←", `v` = "↓")
      bot_idx = which(after == "@", arr.ind=TRUE)
      if (nrow(bot_idx) > 1L) stop("Found >1 bot!")
      after[bot_idx + un_step] = arr
      all_lines = mat_collapse(after)
    }
  } else {
    all_lines = mat_collapse(before)
  }
  writeLines(all_lines)
  cat("\n")
}

movements = list(
  `<` = c(0, -1L),
  `^` = c(-1L, 0L),
  `>` = c(0L, 1L),
  `v` = c(1L, 0L)
)

gap = which(!nzchar(input))

rules = input |>
  tail(-gap) |>
  strsplit(NULL) |>
  unlist()

## PART ONE

map = head(input, gap-1L) |>
  strsplit(NULL) |>
  do.call(what = rbind)

bot_idx = which(map == "@", arr.ind=TRUE)

for (rule in rules) {
  # before = map
  step = movements[[rule]]
  switch(map[bot_idx + step],
    `.` = {
      map[bot_idx] = "."
      bot_idx = bot_idx + step
      map[bot_idx] = "@"
    },
    `#` = {
      # do nothing
    },
    `O` = {
      k = 2L
      repeat {
        switch(map[bot_idx + k*step],
          `.` = {
            shifted_idx = do.call(rbind, sapply(0:k, \(kk) bot_idx + kk*step, simplify=FALSE))
            shifted = map[shifted_idx]
            map[shifted_idx] = c(tail(shifted, 1L), head(shifted, -1L))
            bot_idx = bot_idx + step
            break
          },
          `O` = { k = k + 1L },  # keep looking
          `#` = break # do nothing: move impossible
        )
      }
    }
  )
  # show_snapshot(before, map, rule)
}

box_idx = which(map == "O", arr.ind=TRUE)

sum(100L*(box_idx[,"row"]-1L) + box_idx[,"col"]-1L)

## PART TWO

paired_idx = function(box_idx, box_side = NULL) {
  if (is.null(box_side)) box_side = wide_map[box_idx]
  box_idx + cbind(0, ifelse(box_side == "[", 1L, -1L))
}

build_wide_map = function(lines, file=NULL) {
  map_widener = list(
    `#` = c("#", "#"),
    `O` = c("[", "]"),
    `.` = c(".", "."),
    `@` = c("@", ".")
  )

  if (!is.null(file)) {
    lines = readLines(file)
    lines = head(lines, which(!nzchar(input))-1L)
  }
  lines |>
    strsplit(NULL) |>
    lapply(\(row) unlist(map_widener[row], use.names=FALSE)) |>
    do.call(what = rbind)
}

wide_map = build_wide_map(head(input, gap-1L))

bot_idx = which(wide_map == "@", arr.ind=TRUE)

for (rule in rules) {
  # before = wide_map
  step = movements[[rule]]
  axis_dir = if (rule %in% c(">", "<")) "hor" else "ver"
  next_obj = wide_map[bot_idx + step]

  switch(next_obj,
    `.` = {
      wide_map[bot_idx] = "."
      bot_idx = bot_idx + step
      wide_map[bot_idx] = "@"
    },
    `#` = {
      # do nothing
    },
    `[` = , `]` = switch(axis_dir,
      hor = {
        k = 2L
        repeat {
          switch(wide_map[bot_idx + k*step],
            `.` = {
              shifted_idx = do.call(rbind, sapply(0:k, \(kk) bot_idx + kk*step, simplify=FALSE))
              shifted = wide_map[shifted_idx]
              wide_map[shifted_idx] = c(tail(shifted, 1L), head(shifted, -1L))
              bot_idx = bot_idx + step
              break
            },
            `[` = , `]` = { k = k + 1L },  # keep looking
            `#` = break # do nothing: move impossible
          )
        }
      },
      ver = {
        all_box_idx = curr_box_idx = rbind(
          bot_idx + step,
          paired_idx(bot_idx + step, next_obj)
        )
        repeat {
          next_row_idx = sweep(curr_box_idx, 2L, step, `+`)
          next_row_obj = wide_map[next_row_idx]
          if (any(next_row_obj == "#")) break
          next_row_box_idx = which(next_row_obj %in% c("[", "]"))
          if (!length(next_row_box_idx)) {
            box_to_idx = sweep(all_box_idx, 2L, step, `+`)
            for (jj in nrow(all_box_idx):1L) {
              wide_map[box_to_idx[jj,, drop=FALSE]] = wide_map[all_box_idx[jj,, drop=FALSE]]
              wide_map[all_box_idx[jj,, drop=FALSE]] = "."
            }
            wide_map[bot_idx] = "."
            bot_idx = bot_idx + step
            wide_map[bot_idx] = "@"
            break
          }
          curr_box_idx = next_row_idx[next_row_box_idx, , drop=FALSE]
          curr_box_idx = unique(rbind(curr_box_idx, paired_idx(curr_box_idx)))
          all_box_idx = rbind(all_box_idx, curr_box_idx)
        }
      }
    )
  )
  # show_snapshot(before, wide_map, rule)
}

box_left_idx = which(wide_map == "[", arr.ind=TRUE)

sum(100L*(box_left_idx[,"row"]-1L) + box_left_idx[,"col"]-1L)
