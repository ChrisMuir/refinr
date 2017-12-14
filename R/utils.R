# Function that attempts to merge common business name suffixes within a
# character string.
business_suffix <- function(vect) {
  vect %>%
    {gsub(" incorporated| incorporate", " inc", .)} %>%
    {gsub(" corporation| corporations", " corp", .)} %>%
    {gsub(" company| companys| companies", " co", .)} %>%
    {gsub(" limited liability co", " llc", ., fixed = TRUE)} %>%
    {gsub(" limited$", " ltd", .)} %>%
    {gsub(" division| divisions", " div", .)} %>%
    {gsub(" enterprises| enterprise", " ent", .)} %>%
    {gsub(" limited partnership", " lp", ., fixed = TRUE)} %>%
    {gsub(" and ", " & ", ., fixed = TRUE)}
}

# For each element of an input character vector, insert a single space
# between each char. This function is meant to mimic function ngram::splitter,
# but is faster due to fewer input checks.
# Arg numgram_thres is a numeric value. After the splitting, any string that
# has length less than this number will be replaced with NA_character_.
char_splitter <- function(vect, numgram_thres) {
  vapply(vect, function(x) {
    x <- paste0(strsplit(x, split = "", fixed = TRUE)[[1]], collapse = " ")
    if (nchar(x) >= numgram_thres) {
      x
    } else {
      NA_character_
    }
  }, character(1), USE.NAMES = FALSE)
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
