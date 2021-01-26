## get the session script

#' Get the session script
#'
#' A command-line (console) tool to fetch the script for a
#' particular session, remove some unnecessary code and place
#' a copy in a file in the main working directory.
#'
#' It also places a copy of the display pdf file in the
#' main working directory as well.
#'
#' @param n integer: the session number
#' @param scripts the sub-folder containint the scripts, .R files
#' @param pdf the sub-folder containint the pdf files, .pdf
#' @param here where the results should go, normally the working directory
#' @param x An object of class "session"
#' @param ... extra arguments (currently ignored)
#'
#' @return A character string vector which when printed yields
#'         the required pared-down script file.
#' @export
#'
#' @examples
#' \dontrun{
#' session(1)
#' }
session <- function (n, scripts = "scripts", pdf = "PDF", here = getwd()) {
  stopifnot(length(n) == 1, is.numeric(n), n > 0, n < 21, n %% 1 == 0)
  no <- gsub(" ", 0, format(n, width = 3))
  Rscript <- dir(file.path(here, scripts),
                 pattern = paste0(no, "_.*\\.R$"), full.names = TRUE)
  stopifnot(length(Rscript) == 1, file.exists(Rscript))
  txt <- readLines(Rscript)
  l0 <- grep("^### R settings$", txt) - 2
  txt <- txt[-(1:l0)]
  l0 <- grep("^## ----setFigPath,include=FALSE", txt)
  l1 <- grep("^session <-", txt)
  txt <- txt[-(l0:l1)]
  txt <- grep("^rm\\(session\\)", txt, invert = TRUE, value = TRUE)
  attr(txt, "Number") <- no
  PDFfile <- dir(file.path(here, pdf), pattern = paste0(no, "_.*\\.pdf$"),
                 full.names = TRUE)
  # stopifnot(length(PDFfile) == 1, file.exists(PDFfile))
  attr(txt, "PDF") <- PDFfile
  attr(txt, "here") <- here
  class(txt) <- "session"
  txt
}

#' @rdname session
#' @export
print.session <- function (x, ...) {
  here <- attr(x, "here")
  No <- attr(x, "Number")
  Rscript <- paste0(No, "_session_script.R")
  to <- file.path(here, Rscript)
  cat(x, file = to, sep = "\n")
  cat("File: ", sQuote(Rscript), " is now available.\n")

  PDF <- attr(x, "PDF")
  if(file.exists(PDF)) {
    PDFfile <- paste0(No, "_session_PDF.pdf")
    to <- file.path(here, PDFfile)
    file.copy(PDF, to, overwrite = TRUE)
    cat("File: ", sQuote(PDFfile), " is now visible as well.\n")
  }
  invisible(x)
}
