#' Remove Word-specific markup (comments and tracked changes) from HTML
#'
#' Cleans HTML produced by Microsoft Word by removing comment anchors,
#' removing deleted text, and unwrapping inserted text so only the
#' cleaned content remains.
#'
#' @param input_html A file path to an HTML file or an `xml2::xml_document`.
#'
#' @return A cleaned `xml2::xml_document` with Word-specific markup removed.
#'
#' @examples
#' \dontrun{
#' cleaned <- word_html_remove_markup("document.html")
#' }
#'
#' @export
word_html_remove_markup <- function(input_html) {

  # Parse input
  doc <- if (inherits(input_html, "xml_document")) {
    input_html
  } else {
    xml2::read_html(input_html)
  }

  # Remove comment anchors
  comments <- xml2::xml_find_all(doc, "//*[@class='msocomanchor']")
  if (length(comments) > 0) {
    xml2::xml_remove(comments)
  }

  # Remove deleted text
  deletions <- xml2::xml_find_all(doc, "//*[@class='msoDel']")
  if (length(deletions) > 0) {
    xml2::xml_remove(deletions)
  }

  # Unwrap inserted text (keep contents but drop the wrapper)
  insertions <- xml2::xml_find_all(doc, "//*[@class='msoIns']")

  if (length(insertions) > 0) {
    # Reverse order prevents altering node indexing
    for (i in rev(seq_along(insertions))) {
      node <- insertions[[i]]

      contents <- xml2::xml_contents(node)
      if (length(contents) > 0) {
        xml2::xml_add_sibling(node, contents)
      }
      xml2::xml_remove(node)
    }
  }

  return(doc)
}
