input = do.call(rbind, strsplit(readLines('input-data/04'), NULL))

DIM = dim(input)

## PART ONE

horiz = apply(input, 1L, paste, collapse="")
vert  = apply(input, 2L, paste, collapse="")

# my input is square but let's make it work for rectangles
max_dim = max(DIM)
diag_back = sapply(
  (-max_dim):max_dim, # probably could do max_dim-1, no difference
  \(k) paste(input[row(input) - col(input) == k], collapse="")
)
diag_fwd = sapply(
  1:(2*max_dim), # probably could do 2*(1:max_dim), no difference
  \(k) paste(input[row(input) + col(input) == k], collapse="")
)

all_substr = Filter(nzchar, c(horiz, vert, diag_back, diag_fwd))

# NOT XMAS|SAMX because the string 'XMASAMX' is two hits
match_fwd = gregexpr("XMAS", all_substr, fixed=TRUE)
match_bkd = gregexpr("SAMX", all_substr, fixed=TRUE)

length(Filter(\(x) x>0, c(unlist(match_fwd), unlist(match_bkd))))

## PART TWO

a_idx = input == "A" |>
  which(arr.ind = TRUE) |>
  data.frame() |>
  subset(row > 1L & row < DIM[1L] & col > 1L & col < DIM[2L])

all_corners = expand.grid(
  idx = seq_len(nrow(a_idx)),
  x_offset = c(-1L, 1L),
  y_offset = c(-1L, 1L)
) |>
  sort_by(~idx + x_coord + y_coord) |>
  within({
    x_coord = a_idx$row[idx] + x_offset
    y_coord = a_idx$col[idx] + y_offset
    entry = input[cbind(x_coord, y_coord)]
  })
# Z-curve from top-left
valid_ptn = strsplit(c("MSMS", "MMSS", "SSMM", "SMSM"), NULL)
sum(tapply(
  all_corners$entry,
  all_corners$idx,
  \(corners) Reduce(`||`, lapply(valid_ptn, \(ptn) all(corners == ptn)))
))
