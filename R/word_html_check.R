#' Check a Word-exported HTML document for comments and tracked changes
#'
#' This function inspects a Word-generated HTML file and detects the
#' presence of comments, insertions, and deletions (tracked changes).
#' It accepts either a file path or a parsed `xml2::xml_document` and
#' emits warnings/messages based on what is found.
#'
#' @param input_html A file path to an HTML file or an `xml2::xml_document`
#'   already parsed with `xml2::read_html()`.
#'
#' @return Invisibly returns `NULL`. Called for its side effects
#'   (warnings/messages).
#'
#' @examples
#' \dontrun{
#' word_html_check("document.html")
#' }
#'
#' @export
word_html_check <- function(input_html) {

  # Parse input
  doc <- if (inherits(input_html, "xml_document")) {
    input_html
  } else {
    xml2::read_html(input_html)
  }

  comments   <- xml2::xml_find_all(doc, "//*[@class='msocomanchor']")
  insertions <- xml2::xml_find_all(doc, "//*[@class='msoIns']")
  deletions  <- xml2::xml_find_all(doc, "//*[@class='msoDel']")

  if (length(comments) > 0) {
    warning("This document contains comments")
  }
  if (length(insertions) > 0 || length(deletions) > 0) {
    warning("This document contains tracked changes")
  }
  if (length(comments) == 0 &&
      length(insertions) == 0 &&
      length(deletions) == 0) {
    message("No markup or comments detected in this document")
  }

  invisible(NULL)
}
