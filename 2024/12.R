library(data.table)
library(terra)

input = readLines('input-data/12') |>
  strsplit(NULL) |>
  do.call(what = rbind) |>
  # Otherwise wrap-around regions are found? Possible bug...
  cbind(NA)

# For debugging.
show_rgn = function(rgn, id) {
  mask = matrix(rgn == id, nrow(rgn), ncol(rgn), byrow=TRUE)
  input[!mask] = " "
  bbox = apply(which(mask, arr.ind=TRUE), 2L, range)
  input[
    bbox[1L, "row"]:bbox[2L, "row"],
    bbox[1L, "col"]:bbox[2L, "col"],
    drop = FALSE
  ] |>
    apply(MARGIN = 1L, paste, collapse = "") |>
    writeLines()
  invisible()
}

rst = rast(input, crs="local")
# values=TRUE is from dev version of {terra}!
rgn = patches(rst, values=TRUE)
rgn_poly = as.polygons(rgn)

## PART ONE

region_data = data.table(expanse(rgn, byValue=TRUE, transform=FALSE))
region_data[, perimeter := perim(rgn_poly)]

region_data[, sum(area * perimeter)]

## PART TWO

region_data[, edges := edges(rgn_poly)]
region_data[, sum(area * edges)]
