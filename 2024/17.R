library(data.table)
input = readLines('input-data/17') |>
  Filter(f = nzchar) |>
  tstrsplit(": ") |>
  setDT(key = "V1")

renv = list2env(as.list(sapply(LETTERS[1:3], function(L) {
  input[paste("Register", L), as.integer(V2)]
})))

program = input["Program", as.integer(unlist(strsplit(V2, ",")))]

combo = function(x) switch(x + 1L,
  0L, 1L, 2L, 3L,
  renv$A, renv$B, renv$C,
  stop("Invalid program!")
)

adv = function(x) renv$A = as.integer(renv$A / 2**combo(x))
bdv = function(x) renv$B = as.integer(renv$A / 2**combo(x))
cdv = function(x) renv$C = as.integer(renv$A / 2**combo(x))
bxl = function(x) renv$B = bitwXor(renv$B, x)
bst = function(x) renv$B = combo(x) %% 8L
bxc = function(x) renv$B = bitwXor(renv$B, renv$C)
out = function(x) cat(combo(x) %% 8L, ",", sep="")

jj = 1L
while (jj < length(program)) {
  instr = program[jj]
  operand = program[jj + 1L]

  switch(instr + 1L,
    adv = adv(operand),
    bxl = bxl(operand),
    bst = bst(operand),
    jnz = if (renv$A != 0L) { jj <- operand+1L; next },
    bxc = bxc(operand),
    out = out(operand),
    bdv = bdv(operand),
    cdv = cdv(operand)
  )
  jj = jj + 2L
}
