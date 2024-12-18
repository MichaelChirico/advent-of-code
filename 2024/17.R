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

BXOR = function(x, y) {
  stopifnot(length(x)==1, length(y)==1)
  #if (x < .Machine$integer.max && y < .Machine$integer.max) return(bitwXor(x, y))
  # error if we need more than two divisions
  old = options(warn = 2)
  on.exit(options(old))

  BIG = .Machine$integer.max
  x_bin = c(as.integer(intToBits(x %% BIG)), as.integer(intToBits(x %/% BIG)))
  y_bin = c(as.integer(intToBits(y %% BIG)), as.integer(intToBits(y %/% BIG)))
  sum(2**(0:63) * bitwXor(x_bin, y_bin))
}

program = input["Program", as.integer(unlist(strsplit(V2, ",")))]

run = function(program, stop_early=FALSE, A=NULL, B=NULL, C=NULL) {
  renv = initialize_register(A=A, B=B, C=C)
  # ensure 'renv' gets the right inheritance
  combo = function(x) switch(x + 1L,
    0L, 1L, 2L, 3L,
    renv$A, renv$B, renv$C,
    stop("Invalid program!")
  )

  adv = function(x) renv$A = floor(renv$A / 2**combo(x))
  bdv = function(x) renv$B = floor(renv$A / 2**combo(x))
  cdv = function(x) renv$C = floor(renv$A / 2**combo(x))
  bxl = function(x) renv$B = BXOR(renv$B, x)
  bst = function(x) renv$B = combo(x) %% 8
  bxc = function(x) renv$B = BXOR(renv$B, renv$C)
  out = function(x) renv$out = c(renv$out, combo(x) %% 8)

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
        if (stop_early) if (
          length(renv$out) > length(program)
          || any(renv$out != head(program, length(renv$out)))
        ) break
      },
      bdv = bdv(operand),
      cdv = cdv(operand)
    )
    jj = jj + 2L
  }
  renv
}

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

# binary searches to find range of values producing
#   programs of the right length

lower = 8**(length(program)-2)
upper = 8**(length(program)+1)
repeat {
  A = floor((lower + upper)/2)
  if (length(run(program, A=A)$out) >= length(program)) {
    upper=A
  } else if (length(run(program, A=A+1)$out) == length(program)) {
    break
  } else {
    lower=A
  }
}

min_value = A+1

check = matrix(0L, nrow=3L, ncol=8**5)
for (k in seq_len(8**5)-1) {
  check[, k+1] = run(program, A=k, stop_early=TRUE)$out[1:3]
}
poss_mod32768 = which(apply(check, 2L, \(x) all(x==head(program, 3L))))

k = min_value %/% 8**5
i = 1L
repeat {
  if (k %% 100 == 0L) cat(k, '\n')
  for (i in seq_along(poss_mod32768)) {
    A = 8**5 * k + poss_mod32768[i]
    if (isTRUE(all.equal(run(program, A=A, stop_early=TRUE)$out, program))) stop("A=", A)
  }
  k = k + 1L
}
