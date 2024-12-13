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

region_data[, filled_area := expanse(fillHoles(rgn_poly), transform=FALSE)]
region_data[, hole_area := filled_area - area]
rgn_holes = fillHoles(rgn_poly, inverse=TRUE)
patches(rast(rgn_holes), values=TRUE)

rgn_mat = as.matrix(rgn, wide=TRUE)
n_outer_edges = function(id, area) {
  rgn_idx = which(rgn_mat == id, arr.ind=TRUE)
  # any rectangle --> 4 edges
  if (any(apply(rgn_idx, 2L, \(x) length(unique(x))) == 1L)) return(4L)
  # non-rectangular triomino has 6 edges
  if (area == 3L) return(6L)
  return(NA_integer_)
  browser()
  pos0 = rgn_idx[1L, , drop=FALSE]
}
region_data[, outer_edges := n_outer_edges(.BY$value, area), by=value]
region_data[, edges := outer_edges]
region_data[hole_area > 0L & hole_area == 1L, edges := outer_edges + 4L]
region_data[area <= 2, edges := ]

rgn_mat = matrix(rgn, nrow(input), ncol(input), byrow=TRUE)
