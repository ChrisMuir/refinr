# Function that attempts to merge common business name suffixes within a
# character string.
business_suffix <- function(vect) {
  vect <- gsub(" incorporated| incorporate", " inc", vect, perl = TRUE)
  vect <- gsub(" corporation| corporations", " corp", vect, perl = TRUE)
  vect <- gsub(" company| companys| companies", " co", vect, perl = TRUE)
  vect <- gsub(" limited liability co", " llc", vect, fixed = TRUE)
  vect <- gsub(" limited$", " ltd", vect, perl = TRUE)
  vect <- gsub(" division| divisions", " div", vect, perl = TRUE)
  vect <- gsub(" enterprises| enterprise", " ent", vect, perl = TRUE)
  vect <- gsub(" limited partnership", " lp", vect, fixed = TRUE)
  return(vect)
}

# Modified version of stats:::as.matrix.dist() that doesn't mess with dimnames.
as_matrix <- function (x) {
  size <- attr(x, "Size")
  df <- matrix(0, size, size)
  df[row(df) > col(df)] <- x
  df + t.default(df)
}

# R wrapper for stringdist C function `R_lower_tri()`.`
lower_tri <- function(a, method, weight, p, bt, q, useBytes, nthread) {
  x <- .Call("sd_lower_tri", a, method, weight, p, bt, q, useBytes, nthread)
  attributes(x) <- list(class = "dist", Size = length(a), Diag = TRUE,
                        Upper = TRUE, method = method)
  x
}

# R wrapper for C function `list_lens()` (`list_lens()` calls stringdist C
# function `R_lengths()`).
get_list_lengths <- function(x) {
  .Call("list_lens", x)
}
