input = readLines('input-data/03')

# my first instinct
mul_idx = gregexpr(R"(mul\((?<l>[0-9]{1,3}),(?<r>[0-9]{1,3})\))", input, perl=TRUE)

sum(mapply(input, mul_idx, USE.NAMES=FALSE, FUN=function(str, idx) {
  s = attr(idx, "capture.start")
  n = attr(idx, "capture.length")
  l = as.integer(substring(str, s[,"l"], s[,"l"]+n[,"l"]-1L))
  r = as.integer(substring(str, s[,"r"], s[,"r"]+n[,"r"]-1L))
  sum(l*r)
}))

# aided by rex
library(rex)
match_df = input |>
  re_matches(global=TRUE, locations=TRUE, rex(
    "mul(",
    capture(between(digit, 1, 3), name="l"), ",",
    capture(between(digit, 1, 3), name="r"), ")"
  ))
sum(mapply(input, match_df, USE.NAMES=FALSE, FUN=function(s, DF) with(DF, {
  lv = as.integer(substring(s, l.start, l.end))
  rv = as.integer(substring(s, r.start, r.end))
  sum(lv * rv)
})))

# parse() approach
mul_idx = gregexpr(R"(mul\([0-9]{1,3},[0-9]{1,3}\))", input, perl=TRUE)
mul = `*`
eval(parse(text = paste(collapse = "+", unlist(mapply(input, mul_idx, USE.NAMES=FALSE, SIMPLIFY=FALSE,
  FUN=\(s, idx) substring(s, idx, idx+attr(idx, "match.length")-1L)
)))))

# strsplit() approach
ul=mul
input |>
  strsplit(R"[m(?=ul\([0-9]{1,3},[0-9]{1,3}\))]", perl=TRUE) |>
  unlist() |>
  grep("^ul", x=_, value=TRUE) |> # 1st element might not match
  sub("\\).*", ")", x=_) |>
  paste(collapse = "+") |>
  parse(text = _, keep.source=FALSE) |>
  eval()

