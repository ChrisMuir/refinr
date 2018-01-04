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
  vect <- gsub(" and ", " & ", vect, fixed = TRUE)
  return(vect)
}

# Flatten a nested list such that each character vector occupies its own
# element in the return list. Can handle lists that have inconsistent nesting
# levels.
flatten_list <- function(list_obj) {
  more_lists <- vapply(list_obj, is.list, logical(1), USE.NAMES = FALSE)
  out <- c(list_obj[!more_lists], unlist(list_obj[more_lists], FALSE, FALSE))
  if(sum(more_lists)){
    Recall(out)
  } else {
    return(out)
  }
}
