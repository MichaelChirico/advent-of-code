library(data.table)

build_input = function(f, str=NULL) {
  l = str %||% readLines('input-data/09')
  res = l |>
    strsplit(NULL) |>
    unlist() |>
    as.integer() |>
    c(0L) |>
    matrix(nrow = 2L) |>
    t() |>
    data.table() |>
    setnames(c("filled_blocks", "empty_blocks"))
  res[, left_idx := 1L + cumsum(shift(filled_blocks, fill=0L) + shift(empty_blocks, fill=0L))]
  res[]
}
initialize_dense_file = function(input) {
  n_blocks = nrow(input)
  dense_file_id = rep(NA_integer_, n_blocks)
  dense_file_id[unlist(mapply(\(x, y) seq(x, length.out=y), input$left_idx, input$filled_blocks))] =
    rep(seq_len(n_blocks) - 1L, input$filled_blocks)
  dense_file_id
}

## PART ONE

input = build_input()
dense_file_id = initialize_dense_file(input)

no_data = which(is.na(dense_file_id))
fill_offset = 0L

move_idx = length(dense_file_id)
target_idx = no_data[1L]
move_file_id = nrow(input)

continue = TRUE
while (continue) {
  move_blocks = input$filled_blocks[move_file_id]
  proposed_slots = no_data[fill_offset + seq_len(move_blocks)]
  is_behind = proposed_slots < input$left_idx[move_file_id]
  if (!all(is_behind)) {
    proposed_slots = proposed_slots[is_behind]
    end_idx = input$left_idx[move_file_id] + sum(!is_behind) - 1L
    continue = FALSE
  }
  dense_file_id[proposed_slots] = move_file_id-1L
  fill_offset = fill_offset + move_blocks
  move_file_id = move_file_id - 1L
}

sprintf("%20.0f", sum(head(dense_file_id, end_idx) * (seq_len(end_idx) - 1L)))

## PART TWO
input = build_input()
dense_file_id = initialize_dense_file(input)
input[, gap_size := empty_blocks]
input[, offset := 0L]

# could probably terminate earlier but shrugs
# NB: Really subtle in instructions, but this is the correct iteration
for (move_block_idx in nrow(input):1) {
  n_move = input$filled_blocks[move_block_idx]
  fits = which(head(input$gap_size, move_block_idx-1L) >= n_move)
  if (length(fits)) {
    # wipe old data
    dense_file_id[input$left_idx[move_block_idx] + seq_len(n_move) - 1L] = NA_integer_
    # write moved data
    target_idx = input[fits[1L], left_idx + filled_blocks + offset + seq_len(n_move) - 1L]
    dense_file_id[target_idx] = move_block_idx - 1L
    # update info about remaining gaps
    input[fits[1L], `:=`(gap_size = gap_size - n_move, offset = offset + n_move)]
  }
}

sprintf("%20.0f", sum(dense_file_id * (seq_along(dense_file_id) - 1L), na.rm=TRUE))
