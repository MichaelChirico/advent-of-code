library(data.table)
input = readLines('input-data/17') |>
  Filter(f = nzchar) |>
  tstrsplit(": ") |>
  setDT(key = "V1")

initialize_register = function(A = NULL, B = NULL, C = NULL) {
  renv = list2env(as.list(sapply(LETTERS[1:3], function(L) {
    input[paste("Register", L), as.integer(V2)]
  })))
  if (!is.null(A)) renv$A = A
  if (!is.null(B)) renv$B = B
  if (!is.null(C)) renv$C = C
  renv
}


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
out = function(x) renv$out = c(renv$out, combo(x) %% 8L)

## PART ONE
renv = initialize_register()
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

paste(renv$out, collapse=",")

## PART TWO

# reading the program, the 2nd-last step is the only
#   output, which will produce mod(B, 8). in the previous
#   step, B was re-set to floor(A / 8); A is not update
#   by any 
check = integer(1024)
for (k in 0:1023) {
  renv = initialize_register(A = k)
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
      out = {
        out(operand)
        if (length(renv$out) > length(program)) break
        if (any(renv$out != head(program, length(renv$out)))) break
      },
      bdv = bdv(operand),
      cdv = cdv(operand)
    )
    jj = jj + 2L
  }
  check[k] = renv$out[1L]
}
poss_mod1024 = which(check == 2L)

possibleA = outer(poss_mod1024, 1024*(0:2**17), `+`)
for (ii in seq_along(possibleA)) {
  A = possibleA[ii]
  if (ii %% 10000L == 0L) cat(sprintf("A=%d\n", A))
  renv = initialize_register(A = A)
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
      out = {
        out(operand)
        if (length(renv$out) > length(program)) break
        if (any(renv$out != head(program, length(renv$out)))) break
      },
      bdv = bdv(operand),
      cdv = cdv(operand)
    )
    jj = jj + 2L
  }
  stopifnot(renv$out[1L] == 2L)
  if (isTRUE(all.equal(renv$out, program))) stop("A=", A)
}
