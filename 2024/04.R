input = do.call(rbind, strsplit(readLines('input-data/04'), NULL))

horiz = apply(input, 1L, paste, collapse="")
vert  = apply(input, 2L, paste, collapse="")

## PART ONE

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

## PART TWO

a_idx = gregexpr("A", horiz, fixed=TRUE)
hat_pairs = c(
  M.M = "S.S",
  M.S = "M.S",
  S.M = "S.M",
  S.S = "M.M"
)
hat_idx = lapply(
  hat_pairs,
  \(x) gregexpr(gsub("(.)$", "(?=\\1)", x), horiz, perl=TRUE)
)
names(hat_idx) = hat_pairs

total_matched = 0L
for (row in 2:(length(a_idx) - 1L)) {
  a_col = a_idx[[row]]
  if (length(a_col) == 1L && a_col<0L) next
  for (hat_pair in hat_pairs) {
    hat_col_prev = hat_idx[[hat_pair]][[row - 1L]]
    matched_prev = (a_col - 1L) %in% hat_col_prev
    if (!any(matched_prev)) next
    hat_col_next = hat_idx[[hat_pairs[[hat_pair]]]][[row + 1L]]
    matched_next = (a_col - 1L) %in% hat_col_next
    cat(sprintf("a columns matched for %s:%s: %s\n", hat_pair, hat_pairs[[hat_pair]], toString(a_col[matched_prev & matched_next])))
    total_matched = total_matched + sum(matched_prev & matched_next)
  }
}
