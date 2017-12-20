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
