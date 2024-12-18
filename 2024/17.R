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
  if (x < .Machine$integer.max && y < .Machine$integer.max) return(bitwXor(x, y))
  # error if we need more than two divisions
  old = options(warn = 2)
  on.exit(options(old))

  BIG = .Machine$integer.max
  x_bin = c(as.integer(intToBits(x %% BIG)), as.integer(intToBits(x %/% BIG)))
  y_bin = c(as.integer(intToBits(y %% BIG)), as.integer(intToBits(y %/% BIG)))
  sum(2**(0:63) * bitwXor(x_bin, y_bin))
}

program = input["Program", as.integer(unlist(strsplit(V2, ",")))]

run = function(program, A=NULL, B=NULL, C=NULL, stop_early=FALSE, debug=FALSE) {
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
    if (debug) cat(sprintf(
      "After instruction %2d: (A,B,C)=(%2d, %2d, %2d); out=%s\n",
      jj, renv$A, renv$B, renv$C, paste(renv$out, collapse=",")
    ))
    jj = jj + 2L
  }
  renv
}

## PART ONE
paste(run(program)$out, collapse=",")

## PART TWO

digits = data.table(k = 0:7)

tail_matches = function(x, y, n) all(tail(x, n) == tail(y, n))

base8mat_to_dec = function(x) rowSums(sweep(x, 2L, 8^((ncol(x)-1):0), `*`))

for (iter in length(program):1) {
  setnames(digits, "k", paste0("d", iter))
  digits[, A := base8mat_to_dec(.SD), .SDcols=patterns("^d")]
  digits[, by=.I, out_matches := tail_matches(run(program, A=A)$out, program, length(program)-iter+1L)]
  keep_cols = paste0("d", length(program):iter)
  digits = digits[(out_matches), .(k = 0:7), by=keep_cols]
}

digits[, A := base8mat_to_dec(.SD)]
digits[, sprintf("%30f", min(A))]
