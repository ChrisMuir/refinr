#' Business name suffix normalization
#'
#' Function that attempts to merge common business name suffixes within a
#' character string.
#' @param string Character vector of business names.
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
#' @importFrom dplyr "%>%"
#'
#' @examples \dontrun{
#' business_suffix(c("Acme Inc", "Acme incorporated", "acme company"))
#' [1] "Acme Inc" "Acme inc" "acme co"
#' }
business_suffix <- function(string) {
  stopifnot(is.character(string))

  string <- string %>%
    {gsub(" incorporated| incorporate", " inc", .)} %>%
    {gsub(" corporation| corporations", " corp", .)} %>%
    {gsub(" company| companys| companies", " co", .)} %>%
    {gsub(" limited liability company", " llc", .)} %>%
    {gsub(" limited$", " ltd", .)} %>%
    {gsub(" division| divisions", " div", .)} %>%
    {gsub(" enterprises| enterprise", " ent", .)} %>%
    {gsub(" limited partnership", " lp", .)} %>%
    {gsub(" and ", " & ", .)}
  return(string)
}
