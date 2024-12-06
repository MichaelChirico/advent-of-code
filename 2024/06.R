input = do.call(rbind, strsplit(readLines("input-data/06"), NULL))

# debugging helper: pretty print
show_state = function(walls, steps, guard_idx) {
  x = matrix(".", nrow=nrow(walls), ncol=ncol(walls))
  x[walls] = "#"
  if (mode(steps) == "list") {
    steps = apply(steps, 2L, \(x) lengths(x)>0L)
  }
  x[steps] = "X"
  x[rbind(guard_idx)] = "^"
  rn = format(seq_len(nrow(x)))
  rn = c(strrep(" ", nchar(rn[1L])), rn)
  cn = paste(seq_len(ncol(x)) %% 10L, collapse="")
  writeLines(paste(rn, c(cn, apply(x, 1L, paste, collapse=""))))
  invisible()
}

steps0 = walls0 = matrix(FALSE, nrow=nrow(input), ncol=ncol(input))
walls0[] = input=="#"
dir=0L

steps=steps0; walls=walls0
guard_idx = as.integer(which(input == "^", arr.ind=TRUE))

# rotate 90' counter-clockwise
rot = \(M) t(M[, ncol(M):1])

## PART ONE

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
  # only needed here to figure out which direction we're in at finish for part 2.
  dir = (dir+1L) %% 4L
}

sum(steps)

## PART TWO

loop_blocks = 0L

# only possible places for a new obstacle must come where steps originally fell
for (ii in seq_len((4L-dir)%%4L)) steps=rot(steps) # get back to 'input' orientation
block_proposals = which(steps, arr.ind=TRUE)
for (proposal_idx in seq_len(nrow(block_proposals))) {
  message(proposal_idx)
  walls = walls0
  walls[block_proposals[proposal_idx, , drop=FALSE]] = TRUE # add the proposed wall
  # record the direction of each step
  steps = matrix(list(), nrow=nrow(steps), ncol=ncol(steps))
  dir = 0L
  guard_idx = which(input == "^", arr.ind=TRUE)

  repeat {
    hits_above = which(walls[1:guard_idx[1L], guard_idx[2L]])
    if (length(hits_above)) {
      # next '#' is the "lowest" one, and we turn _before_ (i.e. 1 row higher)
      next_turn_row = tail(hits_above, 1L) + 1L
    } else {
      next_turn_row = 1L
    }
  
    new_rows = next_turn_row:guard_idx[1L]
    new_col = guard_idx[2L]
    steps[new_rows, new_col] = lapply(steps[new_rows, new_col], append, dir)
    if (any(sapply(steps[new_rows, new_col], anyDuplicated))) {
      loop_blocks = loop_blocks + 1L
      break
    }
  
    if (next_turn_row == 1L) break
  
    guard_idx = c(ncol(walls)+1L-guard_idx[2L], next_turn_row)
    steps = rot(steps); walls = rot(walls)
    dir = (dir+1L) %% 4L
  }
}

loop_blocks
