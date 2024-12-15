input = readLines('input-data/15')

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

walls = matrix(map == "#", nrow(map), ncol(map))
boxes = matrix(map == "O", nrow(map), ncol(map))

bot_idx = which(map == "@", arr.ind=TRUE)

for (rule in rules) {
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
        next_obj = map[bot_idx + k*step]
        switch(next_obj,
          `.` = {
            shifted_idx = do.call(rbind, sapply(0:k, \(kk) bot_idx + kk*step, simplify=FALSE))
            shifted = map[shifted_idx]
            map[shifted_idx] = c(tail(shifted, 1L), head(shifted, -1L))
            bot_idx = bot_idx + step
          },
          `O` = { k = k + 1L },  # keep looking
          `#` = break # do nothing: move impossible
        )
      }
    }
  )
}

box_idx = which(map == "O", arr.ind=TRUE)

sum(100L*(box_idx[,"row"]-1L) + box_idx[,"col"]-1L)
