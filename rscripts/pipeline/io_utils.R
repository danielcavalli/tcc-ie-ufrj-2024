# Utility helpers for managing raw artifact IO in the modular DiD pipeline.

sanitize_label <- function(label) {
  label <- tolower(label)
  label <- gsub("[^a-z0-9]+", "_", label)
  label <- gsub("_+", "_", label)
  label <- gsub("^_|_$", "", label)
  if (nchar(label) == 0) "dataset" else label
}

raw_artifact_root <- function() {
  here::here("data", "outputs", "raw")
}

published_output_root <- function() {
  here::here("data", "outputs")
}

ensure_raw_subdir <- function(...) {
  path <- file.path(raw_artifact_root(), ...)
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  path
}

ensure_published_subdir <- function(...) {
  path <- file.path(published_output_root(), ...)
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  path
}

raw_artifact_path <- function(subdir, filename, create_dir = TRUE) {
  if (create_dir) {
    dir_path <- ensure_raw_subdir(subdir)
  } else {
    dir_path <- file.path(raw_artifact_root(), subdir)
  }
  file.path(dir_path, filename)
}

published_artifact_path <- function(subdir = NULL, filename, create_dir = TRUE) {
  if (is.null(subdir) || subdir == "") {
    dir_path <- published_output_root()
  } else if (create_dir) {
    dir_path <- ensure_published_subdir(subdir)
  } else {
    dir_path <- file.path(published_output_root(), subdir)
  }
  file.path(dir_path, filename)
}

publish_copy <- function(source_path,
                         subdir = NULL,
                         filename = NULL,
                         overwrite = TRUE) {
  if (is.null(source_path) || all(is.na(source_path)) || !file.exists(source_path)) {
    return(NA_character_)
  }
  if (is.null(filename)) {
    filename <- basename(source_path)
  }
  dest_path <- published_artifact_path(subdir, filename)
  if (file.exists(dest_path) && !overwrite) {
    stop("Published artifact already exists at ", dest_path)
  }
  file.copy(source_path, dest_path, overwrite = TRUE)
  dest_path
}

write_published_csv <- function(object,
                                filename,
                                subdir = NULL,
                                overwrite = TRUE,
                                ...) {
  dest_path <- published_artifact_path(subdir, filename)
  if (file.exists(dest_path) && !overwrite) {
    stop("Published artifact already exists at ", dest_path)
  }
  readr::write_csv(object, dest_path, ...)
  dest_path
}

write_artifact <- function(object, subdir, filename, overwrite = TRUE, quiet = FALSE, ...) {
  path <- raw_artifact_path(subdir, filename)
  if (!overwrite && file.exists(path)) {
    stop("Artifact already exists at ", path, " and overwrite = FALSE")
  }

  ext <- tolower(tools::file_ext(path))
  if (!quiet) {
    cli::cli_alert_info("Writing artifact: {path}")
  }

  switch(ext,
    "rds" = saveRDS(object, path, ...),
    "csv" = readr::write_csv(object, path, ...),
    "qs" = {
      if (!requireNamespace("qs", quietly = TRUE)) {
        stop("qs package required to write .qs artifacts")
      }
      qs::qsave(object, path, ...)
    },
    "json" = {
      if (!requireNamespace("jsonlite", quietly = TRUE)) {
        stop("jsonlite package required to write .json artifacts")
      }
      jsonlite::write_json(object, path, auto_unbox = TRUE, ...)
    },
    stop("Unsupported artifact extension: ", ext)
  )
  path
}

read_artifact <- function(subdir, filename, ...) {
  path <- raw_artifact_path(subdir, filename, create_dir = FALSE)
  if (!file.exists(path)) {
    stop("Artifact not found at ", path)
  }

  ext <- tolower(tools::file_ext(path))
  switch(ext,
    "rds" = readRDS(path, ...),
    "csv" = readr::read_csv(path, show_col_types = FALSE, ...),
    "qs" = {
      if (!requireNamespace("qs", quietly = TRUE)) {
        stop("qs package required to read .qs artifacts")
      }
      qs::qread(path, ...)
    },
    "json" = {
      if (!requireNamespace("jsonlite", quietly = TRUE)) {
        stop("jsonlite package required to read .json artifacts")
      }
      jsonlite::read_json(path, simplifyVector = TRUE, ...)
    },
    stop("Unsupported artifact extension: ", ext)
  )
}

artifact_exists <- function(subdir, filename) {
  file.exists(raw_artifact_path(subdir, filename, create_dir = FALSE))
}

include_metadata <- function(path, metadata = list()) {
  if (length(metadata) == 0) {
    return(invisible(NULL))
  }

  meta_path <- paste0(path, ".meta.json")
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite package required to write metadata")
  }
  jsonlite::write_json(metadata, meta_path, auto_unbox = TRUE, pretty = TRUE)
  meta_path
}

