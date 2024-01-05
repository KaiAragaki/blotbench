#' Convert a .scn file to another format
#'
#' The BioRad ChemiDoc produces .scn files that can be converted to .tif(f)
#' files using bfconvert.
#'
#' This function requires [bftools](https://bio-formats.readthedocs.io/en/stable/users/comlinetools/index.html) to be installed
#'
#' @param file Character. Path to the file to be converted.
#' @param dest_name Character. File name and path to save to. If not supplied,
#'   will be same name and location as input, but with a .tif extension.
#' @param overwrite Logical. Should the file be overwritten if it already
#'   exists?
#'
#' @return Path to the saved file
#' @export
#'
#' @examples
#' \dontrun{
#' wb_convert_scn("path/to/file.scn", dest_name = "~/new/path.tif")
#' }
wb_convert_scn <- function(file, dest_name = NULL, overwrite = FALSE) {
  check_if_bfconvert_exists()

  if (is.null(dest_name))
    dest_name <- paste0(fs::path_ext_remove(file), ".tif")

  cmd <- "bfconvert '{file}' '{dest_name}'"

  if (fs::file_exists(dest_name) && !overwrite) {
    rlang::abort("File exists and overwrite is not TRUE")
  }

  if (overwrite) cmd <- paste(cmd, "-overwrite")
  system(glue::glue(cmd))
  dest_name
}

check_if_bfconvert_exists <- function() {
  exists <- suppressWarnings(
    system2(
      "command", c("-v", "bfconvert"),
      stdout = FALSE,
      stderr = FALSE
    )
  )
  # Returns 0 if it exists

  if (exists != 0) {
    cli::cli_abort(
      c(
        "Could not find bfconvert",
        i = "bfconvert doesn't appear to be installed on this machine",
        i = "It might not be in your PATH",
        i = "You can download it (and all of bftools) here:",
        i = "https://bio-formats.readthedocs.io/en/stable/users/comlinetools/index.html" #nolint
      )
    )
  }
}
