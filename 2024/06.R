input = do.call(rbind, strsplit(readLines("input-data/06"), NULL))
steps = walls = matrix(FALSE, nrow=nrow(input), ncol=ncol(input))
walls[] = input=="#"

guard_idx = as.integer(which(input == "^", arr.ind=TRUE))

show_state = function(walls, steps, guard_idx) {
  x = matrix(".", nrow=nrow(walls), ncol=ncol(walls))
  x[walls] = "#"
  x[steps] = "X"
  x[rbind(guard_idx)] = "^"
  rn = format(seq_len(nrow(x)))
  rn = c(strrep(" ", nchar(rn[1L])), rn)
  cn = paste(seq_len(ncol(x)) %% 10L, collapse="")
  writeLines(paste(rn, c(cn, apply(x, 1L, paste, collapse=""))))
  invisible()
}

# rotate 90' counter-clockwise
rot = \(M) t(M[, ncol(M):1])

repeat {
  hits_above = which(walls[1:guard_idx[1L], guard_idx[2L]])
  if (length(hits_above)) {
    # next '#' is the "lowest" one, and we turn _before_ (i.e. 1 row higher)
    next_turn_row = tail(hits_above, 1L) + 1L
  } else {
    next_turn_row = 1L
  }

  steps[cbind(next_turn_row:guard_idx[1L], guard_idx[2L])] = TRUE

  if (next_turn_row == 1L) break

  guard_idx = c(ncol(walls)+1L-guard_idx[2L], next_turn_row)
  steps = rot(steps); walls = rot(walls)
}

sum(steps)
