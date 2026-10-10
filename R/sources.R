verify_sources <- function() {
  found <- purrr::map_chr(names(raw_files), \(f) digest::digest(file = file.path(raw_dir, f), algo = "sha256"))
  bad <- names(raw_files)[found != raw_files]
  if (length(bad) > 0) stop("Hash mismatch: ", paste(bad, collapse = ", "))
  invisible(TRUE)
}
