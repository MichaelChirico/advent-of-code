input = do.call(rbind, strsplit(readLines('input-data/04'), NULL))

horiz = apply(input, 1L, paste, collapse="")
vert  = apply(input, 2L, paste, collapse="")

# my input is square but let's make it work for rectangles
max_dim = max(dim(input))
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
