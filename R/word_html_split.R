#' Split an HTML document into multiple files based on heading levels
#'
#' Takes an HTML document (Word-generated or otherwise) and splits it
#' into multiple component files. Each component begins at a specified
#' heading tag (e.g. "h1") and continues until the next such heading.
#'
#' @param input_html A file path or a parsed `xml2::xml_document`.
#' @param output_dir Directory where split HTML files will be written.
#' @param heading_tag A heading tag ("h1", "h2", etc.) used to define sections.
#' @param wrap_html Logical; if TRUE, each section is written as a minimal
#'   HTML document. If FALSE, the raw fragment is written.
#'
#' @return Invisibly returns `TRUE`. Called for its side effect of writing files.
#'
#' @examples
#' \dontrun{
#' word_html_split("document.html", output_dir = "components")
#' }
#'
#' @export
word_html_split <- function(
    input_html,
    output_dir = "components",
    heading_tag = "h1",
    wrap_html = FALSE
) {

  # Parse input
  doc <- if (inherits(input_html, "xml_document")) {
    input_html
  } else {
    xml2::read_html(input_html)
  }

  # Identify heading nodes
  headings <- rvest::html_elements(doc, heading_tag)

  if (length(headings) == 0) {
    stop(sprintf("No <%s> tags found in document", heading_tag))
  }

  # All siblings of the first heading
  body_nodes <- xml2::xml_siblings(headings[[1]])

  # Locate heading positions
  heading_positions <- which(rvest::html_name(body_nodes) == heading_tag)

  # Create output directory
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Process each section
  for (i in seq_along(heading_positions)) {

    start <- heading_positions[i]
    end <- if (i < length(heading_positions)) {
      heading_positions[i + 1] - 1
    } else {
      length(body_nodes)
    }

    section_nodes <- body_nodes[start:end]

    # Extract title for filename
    title <- rvest::html_text(section_nodes[1], trim = TRUE)
    title <- stringr::str_squish(title)

    safe_title <- gsub("[^A-Za-z0-9_-]+", "_", title)
    safe_title <- sub("_+$", "", safe_title)

    if (nchar(safe_title) <= 1) {
      safe_title <- paste0("Unnamed_section_", i)
    }

    file_path <- file.path(output_dir, paste0(safe_title, ".html"))

    if (wrap_html) {
      out_doc <- xml2::read_html("<html><body></body></html>")
      body_el <- rvest::html_element(out_doc, "body")

      xml2::xml_add_child(body_el, section_nodes)
      xml2::write_html(out_doc, file_path)

    } else {
      writeLines(as.character(section_nodes), file_path)
    }
  }

  invisible(TRUE)
}
