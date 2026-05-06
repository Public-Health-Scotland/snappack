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
    apply_label = FALSE,
    sensitivity = c("Personal", "OFFICIAL", "OFFICIAL_SENSITIVE_VMO"),
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

  sensitivity <- match.arg(sensitivity)

  ## ---- build message components -----------------------------------------

  boundary <- paste0("----=_R_", sample(1e8, 1))


  sensitivity_headers <- character()

  if (isTRUE(apply_label)) {

    xml <- phstemplates:::sensitivity_label_xml[[sensitivity]]

    if (is.null(xml)) {
      stop("Unknown sensitivity label: ", sensitivity, call. = FALSE)
    }

    mip <- .extract_mip_label_from_xml(xml)

    set_date <- format(
      as.POSIXct(Sys.time(), tz = "UTC"),
      "%Y-%m-%dT%H:%M:%OS3Z"
    )

    msip_parts <- c(
      paste0("MSIP_Label_", mip$guid, "_Enabled=True"),
      paste0("MSIP_Label_", mip$guid, "_SiteId=", mip$tenant_id),
      paste0("MSIP_Label_", mip$guid, "_SetDate=", set_date),
      paste0("MSIP_Label_", mip$guid, "_Name=", sensitivity),
      paste0("MSIP_Label_", mip$guid, "_ContentBits=", mip$contentBits),
      paste0("MSIP_Label_", mip$guid, "_Method=", mip$method)
    )

    sensitivity_headers <- c(
      paste0(
        "X-MS-Exchange-Organization-ModifySensitivityLabel: ",
        "00000000-0000-0000-0000-000000000000;",
        mip$guid
      ),
      paste0(
        "msip_labels: ",
        paste(msip_parts, collapse = ";"),
        ";"
      )
    )
  }


  headers <- c(
    "X-Unsent: 1",
    paste0("From: ", from),
    paste0("To: ", to),
    if (!is.null(cc)) paste0("Cc: ", cc),
    paste0("Subject: ", subject),
    paste0("Date: ", format(Sys.time(), "%a, %d %b %Y %H:%M:%S %z")),
    "MIME-Version: 1.0",
    sensitivity_headers,
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

#' Internal helper for getting sensitivity labels from phstemplates
#'
#' @param xml_string should be phstemplates:::sensitivity_label_xml
#'
#' @returns list of label components
#' @keywords internal
#' @noRd
.extract_mip_label_from_xml <- function(xml_string) {

  attrs <- regmatches(
    xml_string,
    gregexpr('(?<=\\s)[a-zA-Z]+="[^"]+"', xml_string, perl = TRUE)
  )[[1]]

  kv <- setNames(
    sub('^[^=]+="', "", sub('"$', "", attrs)),
    sub('=".*$', "", attrs)
  )

  list(
    guid        = gsub("[{}]", "", kv[["id"]]),
    tenant_id  = gsub("[{}]", "", kv[["siteId"]]),
    contentBits = kv[["contentBits"]],
    method     = kv[["method"]]
  )
}
