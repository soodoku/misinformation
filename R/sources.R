raw_files <- c(
  media_poll_items_2018.csv = "22a61c3847d92d48edb801e52be9a753f43e43f9165a044af6bcb0aa71bc25b3",
  roper_toplines.csv = "8a390f7537c50ab228f3172092cfffbc938577d366c4fc865ec47ad9c0e62bcc",
  mturk_july_2017.csv = "62aa2380baf0fcb4ca7aeb64d552de597c2927d61289d27146072e70f329fa67"
)

verify_sources <- function() {
  found <- purrr::map_chr(names(raw_files), \(f) digest::digest(file = file.path("data", "raw", f), algo = "sha256"))
  bad <- names(raw_files)[found != raw_files]
  if (length(bad) > 0) stop("Hash mismatch: ", paste(bad, collapse = ", "))
  invisible(TRUE)
}
