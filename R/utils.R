#' Business name suffix normalization
#'
#' Function that attempts to merge common business name suffixes within a
#' character string.
#' @param vect Character vector of business names.
#'
#' @details NOTE: This function also edits all instances of the word "and" to
#'   be "&". Here is a complete list of the edits this function will make:
#'   \itemize{
#'   \item "incorporated" and "incorporate" become "inc".
#'   \item "corporation" and  "corporations" become "corp".
#'   \item "company", "companys" and "companies" become "co".
#'   \item "limited liability company" becomes "llc".
#'   \item "limited" becomes "ltd".
#'   \item "division" and  "divisions" become "div".
#'   \item "enterprises" and "enterprise" become "ent".
#'   \item "limited partnership" becomes "lp".
#'   \item "and" becomes "&".
#'   }
#'
#' @return Character vector of business names, with business name suffixes
#'   edited to be normalized.
#' @noRd
#' @importFrom magrittr "%>%"
#'
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


#' Char Splitter
#' For each element of an input character vector, insert a single space
#' between each char. This function is meant to mimic function ngram::splitter,
#' but is faster do to fewer input checks.
#'
#' @param vect character vector.
#' @param numgram_thres numeric value, after the splitting, any string that
#'  has length less than this number will be replaced with NA_character_.
#'
#' @return character vector
#' @noRd
char_splitter <- function(vect, numgram_thres) {
  # For each value of vect, insert spaces between each char.
  vapply(vect, function(x) {
    x <- paste0(strsplit(x, split = "", fixed = TRUE)[[1]], collapse = " ")
    if (nchar(x) >= numgram_thres) {
      x
    } else {
      NA_character_
    }
  }, character(1), USE.NAMES = FALSE)
}


#' Flatten a nested list such that each character vector occupies its own
#' element in the return list. Can handle lists that have inconsistent nesting
#' levels.
#'
#' @param list_obj list object.
#'
#' @return flattened list object.
#' @noRd
flatten_list <- function(list_obj) {
  more_lists <- vapply(list_obj, is.list, logical(1), USE.NAMES = FALSE)
  out <- c(list_obj[!more_lists], unlist(list_obj[more_lists], FALSE, FALSE))
  if(sum(more_lists)){
    Recall(out)
  }else{
    return(out)
  }
}
