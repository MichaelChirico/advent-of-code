input = readChar('input-data/03', file.size('input-data/03'))

## PART ONE

# my first instinct
mul_idx = gregexpr(R"(mul\((?<l>[0-9]{1,3}),(?<r>[0-9]{1,3})\))", input, perl=TRUE)[[1L]]

s = attr(mul_idx, "capture.start")
n = attr(mul_idx, "capture.length")
l = as.integer(substring(input, s[,"l"], s[,"l"]+n[,"l"]-1L))
r = as.integer(substring(input, s[,"r"], s[,"r"]+n[,"r"]-1L))
sum(l*r)

# aided by rex
library(rex)
match_df = input |>
  re_matches(global=TRUE, locations=TRUE, rex(
    "mul(",
    capture(between(digit, 1, 3), name="l"), ",",
    capture(between(digit, 1, 3), name="r"), ")"
  )) |>
  unlist(recursive = FALSE)
with(match_df, {
  lv = as.integer(substring(input, l.start, l.end))
  rv = as.integer(substring(input, r.start, r.end))
  sum(lv * rv)
})

# parse() approach
mul_idx = gregexpr(R"(mul\([0-9]{1,3},[0-9]{1,3}\))", input, perl=TRUE)[[1L]]
mul = `*`
input |>
  substring(mul_idx, mul_idx+attr(mul_idx, "match.length")-1L) |>
  paste(collapse = "+") |>
  parse(text = _, keep.source = FALSE) |>
  eval()

## PART TWO

toggle_idx = gregexpr("(?:do|don't)\\(\\)", input)[[1L]]
is_do = c(TRUE, attr(toggle_idx, "match.length") == 4L)
recent_toggle = findInterval(mul_idx, c(0L, toggle_idx))
enabled = is_do[recent_toggle]
input |>
  substring(
    mul_idx[enabled],
    mul_idx[enabled]+attr(mul_idx, "match.length")[enabled]-1L
  ) |>
  paste(collapse = "+") |>
  parse(text = _, keep.source = FALSE) |>
  eval()
