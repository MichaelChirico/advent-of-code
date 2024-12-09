input = readLines('input-data/09') |>
  strsplit(NULL) |>
  unlist() |>
  as.integer() |>
  c(0) |>
  matrix(nrow = 2L) |>
  t() |>
  data.frame() |>
  setNames(c("filled_blocks", "empty_blocks"))
input$left_idx = c(1L, with(input, cumsum(head(filled_blocks, -1L) + head(empty_blocks, -1L)) + 1L))
n_blocks = nrow(input)

dense_file_id = rep(NA_integer_, n_blocks)
dense_file_id[unlist(mapply(\(x, y) seq(x, length.out=y), input$left_idx, input$filled_blocks))] =
  rep(seq_len(n_blocks) - 1L, input$filled_blocks)

no_data = which(is.na(dense_file_id))
fill_offset = 0L

move_idx = length(dense_file_id)
target_idx = no_data[1L]
move_file_id = n_blocks

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

input = '2333133121414131402'