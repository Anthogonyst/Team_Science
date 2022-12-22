library(magrittr)
library(stringr)
library(seqinr)
library(XML)
library(jsonlite)

#' The mhtml file splits the file by line length and appends "=" where it exceeds a certain length
#'
#' @param x A chr vector
#' @examples
#' \dontrun{
#' before = "some mhtml stuff="
#' after = "some mhtml stuff"
#' 
#' .DetectLineEndings(before) == after
#' 
#' }
#' @author Anthogonyst
#' 
.DetectLineEndings <- function(x) {
  if (stringr::str_sub(x, -1) == "=") {
    stringr::str_length(x) - 1 }
  else {
    stringr::str_length(x)
  }
}

#' Vectorizes the cleaning function and gets rid of the detected line endings
#' @param x A list of chr vector
#' @examples
#' \dontrun{
#' before = list("first=", "second=", "third=")
#' after = list("first", "second", "third")
#' 
#' all(mapply(.FixMHtml(before), after, FUN = `==`))
#' 
#' }
#' @author Anthogonyst
#' 
.FixMHtml <- function(x) {
  stringr::str_sub(x, 1, sapply(x, .DetectLineEndings))
}

#' Cleans a mthml file and produces a minified html and json file in the same folder, unless optionally specified
#' @param fp The filepath to a mhtml file
#' @param out The filepath to produce the output, default is the same folder
#' @param lhs A html regex for subsetting raw mhtml files
#' @param rhs A html regex for subsetting raw mhtml files
#' @param combined A html regex for cleaning the result
#' @examples
#' \dontrun{
#' file = "data/indeed/1Data Scientist Jobs, Employment in Austin, TX _ Indeed.com.mhtml"
#' optionalFolder = "data/cleaned_file"
#' 
#' CleanMHtml(file, optionalFolder)
#' 
#' file.exists("data/cleaned_file-mini.html")
#' file.exists("data/cleaned_file-mini.json")
#' 
#' }
#' @author Anthogonyst
#' @export
CleanMHtml <- function(fp, out = gsub("\\..*$", "", fp), lhs, rhs, combined) {
  raw = readLines(fp)
  
  body = raw %>%
    .FixMHtml(.) %>% 
      gsub('3D\\"', '"', .) %>%
        seqinr::c2s(.) %>%
          gsub(paste0(".*", lhs, "(.+)", rhs, ".*"), combined, .)
  
  writeLines(body, paste0(out, "-mini.html"))
  
  XML::htmlParse(body) %>%
    XML::xmlToList(.) %>%
      jsonlite::write_json(., paste0(out, "-mini.json"))
  
  return(c(paste0(out, "-mini.html"), paste0(out, "-mini.json")))
}

.CleanSavedLinkedinJobs <- function(fp, out = gsub("\\..*$", "", fp)) {
  
  lhsHtml = '<div class="jobs-box'
  rhsHtml = '<footer class="artdeco-card__actions">'
  combinedHtml = '<div class="jobs-box\\1</div>'
  
  CleanMHtml(fp, out, lhsHtml, rhsHtml, combinedHtml) 
}

.CleanLinkedinPosting <- function(fp, out = gsub("\\..*$", "", fp)) {
  lhsHtml = '<div id="jobsearch-ViewjobPaneWrapper"'
  rhsHtml = 'Return to Search Result</button></div>'
  combinedHtml = '<div\\1</button></div>'
  
  CleanMHtml(fp, out, lhsHtml, rhsHtml, combinedHtml) 
}

#' Cleans the mthml files inside the specified /data/ folder
#' @param dataFolder The filepath holding all of your mhtml files
#' @param outFolder The filepath to produce the output, default is the same folder
#' @param mhtmlFUN A closure with the mhtml cleaning procedure
#' @examples
#' \dontrun{
#' folder = "data/linkedin/"
#' optionalFolder = "data/cleaned_linkedin"
#' 
#' CleanerRunner(folder, optionalFolder)
#' 
#' }
#' @author Anthogonyst
#' @export
CleanerRunner <- function(dataFolder = "data/indeed/", outFolder = "data/cleaned_indeed/", mhtmlFUN = .CleanLinkedinPosting) {
  if (! dir.exists(outFolder))
    dir.create(outFolder)
  
  list.files(dataFolder, pattern = "\\.mhtml$", full.names = TRUE) %>%
    { list(., paste0(outFolder, "/", gsub("\\.mhtml", "", basename(.)))) } %>%
      { mapply(.[[1]], .[[2]], FUN = mhtmlFUN, USE.NAMES = FALSE) }
}
  