#' Save draft email in eml format to be sent from Outlook. Optionally attach one or more files.
#'
#' @param to recipient email address
#' @param from sender email address
#' @param cc Optional. CC email addresses
#' @param subject email subject
#' @param body_html character string containing HTML email contents, or path to HTML file
#' @param attach_files path to attachment file, or character vector of paths
#' @param output_file location to save .eml draft
#' @param quietly logical. Whether to suppress 'email saved' message
#'
#' @returns NULL, invisibly
#' @export
write_eml <- function(
    to,
    from,
    cc = NULL,
    subject,
    body_html,
    attach_files = NULL,
    output_file,
    quietly = FALSE
) {

  ## ---- normalise & validate inputs --------------------------------------

  if (!is.null(attach_files)) {
    attach_files <- as.character(attach_files)
  }

  ## allow body_html to be a file path
  if (length(body_html) == 1 && file.exists(body_html)) {
    body_html <- paste(readLines(body_html, warn = FALSE), collapse = "\n")
  }

  ## ensure .eml extension
  if (tools::file_ext(output_file) == "") {
    output_file <- paste0(output_file, ".eml")
  }

  if (tolower(tools::file_ext(output_file)) != "eml") {
    stop("Incompatible extension in output filename (must be .eml)", call. = FALSE)
  }

  boundary <- paste0("----=_R_", sample(1e8, 1))

  headers <- c(
    "X-Unsent: 1",
    paste0("From: ", from),
    paste0("To: ", to),
    if (!is.null(cc)) paste0("Cc: ", cc),
    paste0("Subject: ", subject),
    paste0("Date: ", format(Sys.time(), "%a, %d %b %Y %H:%M:%S %z")),
    "MIME-Version: 1.0",
    paste0("Content-Type: multipart/mixed; boundary=\"", boundary, "\""),
    ""
  )

  body_part <- c(
    paste0("--", boundary),
    "Content-Type: text/html; charset=UTF-8",
    "",
    body_html,
    ""
  )

  ## ---- attachments ------------------------------------------------------

  attachment_parts <- character()

  if (!is.null(attach_files)) {

    attachment_parts <- unlist(
      lapply(attach_files, function(path) {

        if (!file.exists(path)) {
          stop("Attachment does not exist: ", path, call. = FALSE)
        }

        raw <- readBin(path, "raw", n = file.info(path)$size)
        b64 <- base64enc::base64encode(raw)

        wrapped <- paste(
          substring(b64, seq(1, nchar(b64), 76),
                    seq(76, nchar(b64) + 75, 76)),
          collapse = "\r\n"
        )

        name <- basename(path)
        ext  <- tolower(tools::file_ext(path))

        ## MIME type mapping
        content_type <- switch(
          ext,
          # spreadsheets
          xls   = "application/vnd.ms-excel",
          xlsx  = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
          csv   = "text/csv",
          tsv   = "text/tab-separated-values",

          # documents
          doc   = "application/msword",
          docx  = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
          pdf   = "application/pdf",

          # images
          png   = "image/png",
          jpg   = "image/jpeg",
          jpeg  = "image/jpeg",

          # html
          html  = "text/html",
          htm   = "text/html",
          rhtml = "text/html",

          # fallback
          "application/octet-stream"
        )

        c(
          paste0("--", boundary),
          paste0(
            "Content-Type: ",
            content_type,
            "; name=\"", name, "\""
          ),
          paste0(
            "Content-Disposition: attachment; filename=\"", name, "\""
          ),
          "Content-Transfer-Encoding: base64",
          "",
          wrapped,
          ""
        )
      })
    )
  }

  closing <- paste0("--", boundary, "--")

  ## ---- write file -------------------------------------------------------

  writeLines(
    paste(
      c(headers, body_part, attachment_parts, closing),
      collapse = "\r\n"
    ),
    output_file,
    useBytes = TRUE
  )

  if (!quietly) {
    message("Email saved to ", output_file)
  }

  invisible(NULL)
}
