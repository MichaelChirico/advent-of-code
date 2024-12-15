input = readLines('input-data/15')

show_snapshot = function(before, after = NULL, attempted_rule = NULL) {
  all_lines = apply(before, 1L, paste, collapse="")
  if (!is.null(after)) {
    cat(sprintf("Attempted rule: %s\n", attempted_rule))
    if (isTRUE(all.equal(before, after))) {
      all_lines = c(all_lines, "(cannot move)")
    } else {
      after_lines = apply(after, 1L, paste, collapse="")
      all_lines = paste(all_lines, "->", after_lines)
    }
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

map = head(input, gap-1L) |>
  strsplit(NULL) |>
  do.call(what = rbind)

## PART ONE

map = head(input, gap-1L) |>
  strsplit(NULL) |>
  do.call(what = rbind)

walls = matrix(map == "#", nrow(map), ncol(map))
boxes = matrix(map == "O", nrow(map), ncol(map))

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

map_widener = list(
  `#` = c("#", "#"),
  `O` = c("[", "]"),
  `.` = c(".", "."),
  `@` = c("@", ".")
)
wide_map = head(input, gap-1L) |>
  strsplit(NULL) |>
  lapply(\(row) unlist(map_widener[row], use.names=FALSE)) |>
  do.call(what = rbind)

walls = matrix(wide_map == "#", nrow(wide_map), ncol(wide_map))
box_left = matrix(wide_map == "[", nrow(wide_map), ncol(wide_map))
box_right = matrix(wide_map == "[", nrow(wide_map), ncol(wide_map))

bot_idx = which(wide_map == "@", arr.ind=TRUE)

for (rule in rules) {
  # before = wide_map
  step = movements[[rule]]
  switch(wide_map[bot_idx + step],
    `.` = {
      wide_map[bot_idx] = "."
      bot_idx = bot_idx + step
      wide_map[bot_idx] = "@"
    },
    `#` = {
      # do nothing
    },
    `[` = , `]` = {
      k = 2L
      repeat {
        switch(wide_map[bot_idx + k*step],
          `.` = {
            browser()
            shifted_idx = do.call(rbind, sapply(0:k, \(kk) bot_idx + kk*step, simplify=FALSE))
            shifted = wide_map[shifted_idx]
            wide_map[shifted_idx] = c(tail(shifted, 1L), head(shifted, -1L))
            bot_idx = bot_idx + step
            break
          },
          `[` = , `]` = { browser(); k = k + 1L },  # keep looking
          `#` = break # do nothing: move impossible
        )
      }
    }
  )
  # show_snapshot(before, wide_map, rule)
}

