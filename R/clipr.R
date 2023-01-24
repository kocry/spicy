.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to spicy!")
}


# Determine system type
sys_type <- function() {
  return(Sys.info()["sysname"])
}

#' Is the system clipboard available?
#'
#' Checks to see if the system clipboard is write-able/read-able. This may be
#' useful if you are developing a package that relies on clipr and need to
#' ensure that it will skip tests on machines (e.g. CRAN, Travis) where the
#' system clipboard may not be available.
#'
#' @note This will automatically return `FALSE`, without even performing the
#'   check, if you are running in a non-interactive session. If you must call
#'   this non-interactively, be sure to call using
#'   `clipr_available(allow_non_interactive = TRUE)`, or by setting the
#'   environment variable `CLIPR_ALLOW=TRUE`. **Do not attempt to run
#'   clipr non-interactively on CRAN; this will result in a failed build!**
#'
#' @param \ldots Pass other options to [`clip_write()`]. Generally only used to
#'   pass the argument `allow_non_interactive_use = TRUE`.
#'
#' @return `clipr_available` returns a boolean value.
#'
#' @examples
#' \dontrun{
#' # When using testthat:
#' library(testthat)
#' skip_if_not(clipr_available())
#' }
#'
#' @export
clipr_available <- function(...) {
  clipr_results_check(clipr_available_handler(...))
}

#' @rdname clipr_available
#'
#' @return Prints an informative message to the console with
#'   software and system configuration requirements if clipr is not available
#'   (invisibly returns the same string)
#'
#' @export
dr_clipr <- function(...) {
  res <- clipr_available_handler(...)

  if (clipr_results_check(res)) {
    msg <- msg_clipr_available()
  } else {
    msg <- attr(res$write, which = "condition", exact = TRUE)$message
  }

  message(msg)
  invisible(msg)
}

clipr_available_handler <- function(...) {
  # Do not even do a check unless user has explicitly set CLIPR_ALLOW
  if (!interactive()) {
    clipr_allow <- as.logical(Sys.getenv("CLIPR_ALLOW", "FALSE"))
    if (!clipr_allow) {
      fake_write_attempt <- try(stop("CLIPR_ALLOW has not been set, so clipr will not run interactively"), silent = TRUE)
      return(list(write = fake_write_attempt))
    }
  }
  suppressWarnings({
    read_attempt <- try(clip_read(...), silent = TRUE)
    write_attempt <- try(clip_write(read_attempt, ...), silent = TRUE)
  })
  return(list(read = read_attempt, write = write_attempt))
}

clipr_results_check <- function(res) {
  if (inherits(res$write, "try-error")) return(FALSE)
  if (inherits(res$read, "try-error")) return(FALSE)
  TRUE
}

msg_clipr_available <- function() "clipr has read/write access to the system clipboard!"

msg_no_clipboard <- function() "Clipboard on X11 requires 'xclip' (recommended) or 'xsel'; Clipboard on Wayland requires 'wl-copy' and 'wl-paste'."

msg_no_display <- function() "Clipboard on X11 requires that the DISPLAY envvar be configured."

msg_interactive <- function() "To run clip_write() in non-interactive mode, either call clip_write() with allow_non_interactive = TRUE, or set the environment variable CLIPR_ALLOW=TRUE"

error_interactive <- function() {
  stop(msg_interactive())
}


# Helper function to read from the Windows clipboard
win_read_clip <- function() {
  # On R >= 4.2 (built with UCRT), the default encoding is UTF-8 even on Windows.
  # To avoid the encoding mismatch garbles texts, use 13 (CF_UNICODETEXT),
  format <- if (identical(R.version$crt, "ucrt")) 13 else 1

  utils::readClipboard(format = format)
}

# Helper function to write to the Windows clipboard
win_write_clip <- function(content, object_type, breaks, eos, return_new, ...) {
  format <- if (identical(R.version$crt, "ucrt")) 13 else 1

  .dots <- list(...)

  # If no custom line separator has been specified, use Windows's default
  # newline character '\r\n'
  breaks <- ifelse(is.null(breaks), '\r\n', breaks)

  # If no custom tab separator for tables has been specified, use Windows's
  # default tab character: '\t'
  .dots$sep <- ifelse(is.null(.dots$sep), '\t', .dots$sep)

  # Pass the object to rendering functions before writing out to the clipboard
  rendered_content <- render_object(content, object_type, breaks, .dots)
  utils::writeClipboard(rendered_content, format = format)
  if (return_new) {
    rendered_content
  } else {
    content
  }
}

#' Is the system clipboard available?
#'
#' Checks to see if the system clipboard is write-able/read-able. This may be
#' useful if you are developing a package that relies on clipr and need to
#' ensure that it will skip tests on machines (e.g. CRAN, Travis) where the
#' system clipboard may not be available.
#'
#' @note This will automatically return `FALSE`, without even performing the
#'   check, if you are running in a non-interactive session. If you must call
#'   this non-interactively, be sure to call using
#'   `clipr_available(allow_non_interactive = TRUE)`, or by setting the
#'   environment variable `CLIPR_ALLOW=TRUE`. **Do not attempt to run
#'   clipr non-interactively on CRAN; this will result in a failed build!**
#'
#' @param \ldots Pass other options to [`clip_write()`]. Generally only used to
#'   pass the argument `allow_non_interactive_use = TRUE`.
#'
#' @return `clipr_available` returns a boolean value.
#'
#' @examples
#' \dontrun{
#' # When using testthat:
#' library(testthat)
#' skip_if_not(clipr_available())
#' }
#'
#' @export
clipr_available <- function(...) {
  clipr_results_check(clipr_available_handler(...))
}

#' Read clipboard
#'
#' Read the contents of the system clipboard into a character vector.
#'
#' @param allow_non_interactive By default, clipr will throw an error if run in
#'   a non-interactive session. Set the environment variable
#'   `CLIPR_ALLOW=TRUE` in order to override this behavior.
#'
#' @return A character vector with the contents of the clipboard. If the system
#'   clipboard is empty, returns NULL
#'
#' @note [clip_read()] will not try to guess at how to parse copied text. If
#'   you are copying tabular data, it is suggested that you use
#'   [clip_read_tbl()].
#'
#' @examples
#' \dontrun{
#' clip_text <- clip_read()
#' }
#'
#' @export
clip_read <- function(allow_non_interactive = Sys.getenv("CLIPR_ALLOW", interactive())) {
  if (allow_non_interactive != "TRUE") error_interactive()

  # Determine system type
  sys.type <- sys_type()

  # Use the appropriate handler function
  chosen_read_clip <- switch(sys.type,
                             "Darwin" = osx_read_clip,
                             "Windows" = win_read_clip,
                             X11_read_clip
  )

  content <- chosen_read_clip()

  if (length(content) == 0) {
    warning("System clipboard contained no readable text. Returning NULL.")
    return(NULL)
  }

  content
}


#' Write clipboard
#'
#' Write a character vector to the system clipboard
#'
#' @inheritParams clip_read
#'
#' @param content An object to be written to the system clipboard.
#' @param object_type [clip_write()] tries to be smart about writing objects in a
#'   useful manner. If passed a data.frame or matrix, it will format it using
#'   [write.table()] for pasting into an external spreadsheet program.
#'   It will otherwise coerce the object to a character vector. `auto` will
#'   check the object type, otherwise `table` or `character` can be
#'   explicitly specified.
#' @param breaks The separator to be used between each element of the character
#'   vector being written. `NULL` defaults to writing system-specific line
#'   breaks between each element of a character vector, or each row of a table.
#' @param eos The terminator to be written after each string, followed by an
#'   ASCII `nul`. Defaults to no terminator character, indicated by
#'   `NULL`.
#' @param return_new If true, returns the rendered string; if false, returns the
#'   original object
#' @param ... Custom options to be passed to [write.table()] (if `x` is a
#'   table-like). Defaults to sane line-break and tab standards based on the
#'   operating system. By default, this will use `col.names = TRUE` if the table
#'   object has column names, and `row.names = TRUE` if the object has row names
#'   other than `c("1", "2", "3"...)`. Override these defaults by passing
#'   arguments here.
#'
#' @note On X11 systems, [clip_write()] will cause either xclip (preferred) or
#'   xsel to be called. Be aware that, by design, these processes will fork into
#'   the background. They will run until the next paste event, when they will
#'   then exit silently. (See the man pages for
#'   [xclip](https://linux.die.net/man/1/xclip) and
#'   [xsel](http://www.vergenet.net/~conrad/software/xsel/xsel.1x.html#notes)
#'   for more on their behaviors.) However, this means that even if you
#'   terminate your R session after running [clip_write()], those processes will
#'   continue until you access the clipboard via another program. This may be
#'   expected behavior for interactive use, but is generally undesirable for
#'   non-interactive use. For this reason you must not run [clip_write()] on
#'   CRAN, as the nature of xsel [has caused issues in the
#'   past](https://github.com/mdlincoln/clipr/issues/38).
#'
#'   Call [clipr_available()] to safely check whether the clipboard is readable
#'   and writable.
#'
#' @return Invisibly returns the original object
#'
#' @examples
#' \dontrun{
#' text <- "Write to clipboard"
#' clip_write(text)
#'
#' multiline <- c("Write", "to", "clipboard")
#' clip_write(multiline)
#' # Write
#' # to
#' # clipboard
#'
#' clip_write(multiline, breaks = ",")
#' # write,to,clipboard
#'
#' tbl <- data.frame(a=c(1,2,3), b=c(4,5,6))
#' clip_write(tbl)
#' }
#'
#' @export
clip_write <- function(content, object_type = c("auto", "character", "table"),
                       breaks = NULL, eos = NULL, return_new = FALSE,
                       allow_non_interactive = Sys.getenv("CLIPR_ALLOW", interactive()), ...) {
  if (allow_non_interactive != "TRUE") error_interactive()

  object_type <- match.arg(object_type)
  # Determine system type
  sys.type <- sys_type()

  # Choose an operating system-specific function (stop with error if not
  # recognized)
  chosen_write_clip <- switch(sys.type,
                              "Darwin" = osx_write_clip,
                              "Windows" = win_write_clip,
                              X11_write_clip
  )

  # Supply the clipboard content to write and options list to this function
  invisible(chosen_write_clip(content, object_type, breaks, eos, return_new, ...))
}

#' Clear clipboard
#'
#' Clear the system clipboard.
#'
#' @param \ldots Pass other options to [clip_write()].
#'
#' @note This is a wrapper function for `clip_write("")`
#'
#' @export
clip_clear <- function(...) {
  clip_write(content = "", ...)
}

#' Write contents of the last R expression to the clipboard
#'
#' @param \ldots Pass other options to [clip_write()].
#'
#' @note This is a wrapper function for `clip_write(.Last.value)`
#' @export
clip_write_last <- function(...) {
  clip_write(.Last.value, ...)
}



# Check object type to determine if it will be handled as a simple table or as a
# character vector
render_object <- function(content, object_type, breaks, .dots) {
  if (object_type == "auto")
    object_type <- eval_object(content)
  switch(object_type,
         "table" = table_str(content, breaks, .dots),
         "character" = flat_str(content, breaks))
}

eval_object <- function(content) {
  ifelse(is.data.frame(content) | is.matrix(content), "table", "character")
}

# If object is a table, default to a multiline string with tab separators
table_str <- function(content, breaks, .dots) {
  # Take the system-specific collapse out of the list
  .dots$x <- content
  .dots$sep <- .dots$sep
  .dots$quote <- ifelse(is.null(.dots$quote), FALSE, .dots$quote)
  .dots$na <- ifelse(is.null(.dots$na), "", .dots$na)
  .dots$col.names <- ifelse(is.null(.dots$col.names), !is.null(colnames(content)), .dots$col.names)

  # Check if dataframe rownames are anything different than the default numbered names
  numbered_rownames <- all(rownames(content) == as.character(seq_along(rownames(content))))

  .dots$row.names <- ifelse(is.null(.dots$row.names), ifelse(numbered_rownames, FALSE, !is.null(rownames(content))), .dots$row.names)

  # Writing to and reading from a temp file is much faster than using capture.output
  tbl_file <- tempfile()
  .dots$file = tbl_file
  do.call(utils::write.table, .dots)
  read_tbl <- paste0(readLines(tbl_file), collapse = breaks)
  unlink(tbl_file)

  # If row.names = TRUE and col.names = TRUE, add additional sep character to
  # the start of the table
  if (.dots$row.names & .dots$col.names) {
    read_tbl <- paste0(.dots$sep, read_tbl)
  }

  return(read_tbl)
}

# Helper function to flatten content into 1-tuple character vector (i.e. a
# string)
flat_str <- function(content, breaks) {
  if (typeof(content) != "character") {
    warning("Coercing content to character")
    content <- as.character(content)
  }

  if (length(content) < 1) {
    content <- ""
  } else if (length(content) > 1) {
    content <- paste0(content, collapse = breaks)
  } else if (is.na(content)) {
    content <- "NA"
  }

  return(content)
}

# Determine if a given utility is installed AND accessible
# Takes a character vector whose first element is the name of the
# utility executable and whose subsequent elements are command-line
# arguments to the utility for the test run.
has_util <- function(util_test) {
  if (nzchar(Sys.which(util_test[1]))) {
    # If utility is accessible, check that DISPLAY can be opened.
    try_res <- tryCatch(system2(util_test[1], util_test[-1], stdout = TRUE, stderr = TRUE),
                        error = function(c) {
                          print(c)
                          return(FALSE)
                        },
                        warning = function(c) {
                          print(c)
                          return(FALSE)
                        }
    )

    # In the case of an error/warning on trying the function, then the util is
    # not available
    if (identical(try_res, FALSE)) {
      notify_no_display()
    } else {
      TRUE
    }
  } else {
    FALSE
  }
}

# Determine if system has 'xclip' installed AND it's accessible
has_xclip <- function() has_util(c("xclip", "-o", "-selection", "clipboard"))

# Determine if system has 'xsel' installed
has_xsel <- function() has_util(c("xsel", "--clipboard", "--output"))

# Determine if system has both 'wl-paste' and 'wl-copy' installed
has_wl_clipboard <- function() has_wl_paste() & has_wl_copy()

# Determine if system has 'wl-paste' installed
has_wl_paste <- function() has_util(c("wl-paste", "--primary"))

# Determine if system has 'wl-paste' installed
has_wl_copy <- function() has_util(c("wl-copy", "--primary"))

# Stop read/write and return an error of missing clipboard software.
notify_no_cb <- function() {
  stop(msg_no_clipboard(), call. = FALSE)
}

notify_no_display <- function() {
  stop(msg_no_display(), call. = FALSE)
}

# Helper function to read from the X11 clipboard
#
# Requires the utility 'xclip' or 'xsel' when using X11, or 'wl-paste' when
# using Wayland. This function will stop with an error if neither is found.
# Adapted from:
# https://github.com/mrdwab/overflow-mrdwab/blob/master/R/readClip.R and:
# https://github.com/jennybc/reprex/blob/master/R/clipboard.R
X11_read_clip <- function() {
  if (has_xclip()) {
    con <- pipe("xclip -o -selection clipboard")
  } else if (has_xsel()) {
    con <- pipe("xsel --clipboard --output")
  } else if (has_wl_paste()) {
    con <- pipe("wl-paste")
  } else {
    notify_no_cb()
  }
  content <- scan(con, what = character(), sep = "\n",
                  blank.lines.skip = FALSE, quiet = TRUE)
  close(con)
  return(content)
}

# Helper function to write to the X11 clipboard
#
# Requires the utility 'xclip' or 'xsel' when using X11, or 'wl-copy' when using
# Wayland. This function will stop with an error if neither is found. Adapted
# from https://github.com/mrdwab/overflow-mrdwab/blob/master/R/writeClip.R
#
# Targets "primary" and "clipboard" clipboards if using xclip, see:
# http://unix.stackexchange.com/a/69134/89254
X11_write_clip <- function(content, object_type, breaks, eos, return_new, ...) {
  if (has_xclip()) {
    con <- pipe("xclip -i -sel p -f | xclip -i -sel c", "w")
  } else if (has_xsel()) {
    con <- pipe("xsel --clipboard --input", "w")
  } else if (has_wl_copy()) {
    con <- pipe("wl-copy", "w")
  } else {
    notify_no_cb()
  }

  .dots <- list(...)

  write_nix(content, object_type, breaks, eos, return_new, con, .dots)
}

# Helper function to read from the OS X clipboard
# Adapted from https://github.com/jennybc/reprex/blob/master/R/clipboard.R
osx_read_clip <- function() {
  con <- pipe("pbpaste")
  content <- scan(con, what = character(), sep = "\n",
                  blank.lines.skip = FALSE, quiet = TRUE)
  close(con)
  return(content)
}

# Helper function to write to the OS X clipboard
# Adapted from https://github.com/jennybc/reprex/blob/master/R/clipboard.R
osx_write_clip <- function(content, object_type, breaks, eos, return_new, ...) {
  .dots <- list(...)
  con <- pipe("pbcopy")

  write_nix(content, object_type, breaks, eos, return_new, con, .dots)
}

# The same content rendering and writing steps are used in both OS X and Linux,
# just with different connection objects
write_nix <- function(content, object_type, breaks, eos, return_new, con, .dots) {
  # If no custom line separator has been specified, use Unix's default newline
  # character '\n'
  breaks <- ifelse(is.null(breaks), '\n', breaks)

  # If no custom tab separator for tables has been specified, use Unix's default
  # tab character: '\t'
  .dots$sep <- ifelse(is.null(.dots$sep), '\t', .dots$sep)

  # Pass the object to rendering functions before writing out to the clipboard
  rendered_content <- render_object(content, object_type, breaks, .dots)

  # Suppress pipe() warning when writing an empty string with a NULL string
  # ending.
  if (identical(rendered_content, "")) {
    suppressWarnings(writeChar(rendered_content, con = con, eos = eos))
  } else {
    writeChar(rendered_content, con = con, eos = eos)
  }

  close(con)

  if (return_new) {
    rendered_content
  } else {
    content
  }
}
