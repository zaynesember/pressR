# On-disk archive of scraped releases.
# -----------------------------------
# Releases are stored locally as one xz-compressed RDS per calendar year
# (releases-YYYY.rds), de-duplicated by `url` (a re-scrape refreshes a row).
# Year partitioning keeps each file small, lets read_archive() load only the
# years a date range needs, and gives natural snapshot units for publishing to
# GitHub release assets. The body text dominates size (~960 bytes/release
# compressed, ~33 MB/year), so the archive lives on disk -- not in the package.

# Resolve the archive directory: explicit arg > option > standard user data dir.
archive_dir <- function() {
  getOption("pressR.archive_dir", tools::R_user_dir("pressR", "data"))
}

# Files in an archive directory, in year order.
archive_files <- function(dir) {
  f <- list.files(dir, pattern = "^releases-[0-9]{4}\\.rds$", full.names = TRUE)
  f[order(f)]
}

#' Append scraped releases to the on-disk archive
#'
#' Writes releases into a local, year-partitioned store, de-duplicating by
#' `url` so re-running a scrape refreshes existing rows rather than duplicating
#' them. Builds up historical coverage across runs.
#'
#' @param releases A releases tibble from [scrape_member()], [scrape_pressers()],
#'   or [scrape_house()] (must have at least `date` and `url` columns).
#' @param dir Archive directory. Defaults to `getOption("pressR.archive_dir")`
#'   or, failing that, `tools::R_user_dir("pressR", "data")`.
#' @param quiet Suppress the summary message.
#' @return Invisibly, a list with `dir`, `added`, and `updated` counts.
#' @seealso [read_archive()], [publish_archive()].
#' @export
archive_releases <- function(releases, dir = archive_dir(), quiet = FALSE) {
  if (!is.data.frame(releases)) {
    cli::cli_abort("{.arg releases} must be a data frame.")
  }
  if (!all(c("date", "url") %in% names(releases))) {
    cli::cli_abort("{.arg releases} needs {.field date} and {.field url} columns.")
  }
  releases <- tibble::as_tibble(releases)
  attr(releases, "failures") <- NULL  # don't carry the scrape failures table
  releases$date <- as.Date(releases$date)
  releases <- releases[!is.na(releases$date) & !is.na(releases$url) & nzchar(releases$url), , drop = FALSE]
  if (nrow(releases) == 0) {
    if (!quiet) cli::cli_alert_warning("No archivable releases (need non-NA date and url).")
    return(invisible(list(dir = dir, added = 0L, updated = 0L)))
  }

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  year <- format(releases$date, "%Y")
  added <- 0L
  updated <- 0L

  # Cross-year guard: a source can reuse the same permalink for a genuinely
  # different release (a member's office re-opening an annual award's
  # nominations at the same slug a year later) or a re-scrape can simply
  # resolve a release's date differently than before. Either way, if the url
  # now lands in a different year than where it already lives, the per-year
  # loop below only touches the NEW year's file and leaves a stale copy
  # behind in the old one -- two files, one url. quanteda uses url as the
  # docname, so tag-complete's rbind2 fails ("docnames must be unique") the
  # next time all year files stream together, and the crash lands far from
  # this scrape (a 2026-09-07 refresh crashed a fold-in over a url last
  # written in 2024). Same "new wins" rule as the in-year case, just checked
  # across every file this batch doesn't already own.
  relocated <- character(0)  # urls found living in a year this batch isn't touching
  for (f in archive_files(dir)) {
    fy <- sub("^releases-([0-9]{4})\\.rds$", "\\1", basename(f))
    if (fy %in% year) next  # the main loop below already reconciles this file
    old <- readRDS(f)
    stale <- old$url %in% releases$url
    if (any(stale)) {
      relocated <- c(relocated, old$url[stale])
      saveRDS(old[!stale, , drop = FALSE], f, compress = "xz")
    }
  }
  updated <- updated + length(relocated)

  for (y in unique(year)) {
    new <- releases[year == y, , drop = FALSE]
    path <- file.path(dir, sprintf("releases-%s.rds", y))

    if (file.exists(path)) {
      old <- readRDS(path)
      overlap <- sum(new$url %in% old$url)
      updated <- updated + overlap
      # a url the cross-year guard just relocated already existed in the
      # archive (just under a different year) -- it must not also be
      # counted as newly added here
      added <- added + (nrow(new) - overlap - sum(new$url %in% relocated & !(new$url %in% old$url)))
      old <- old[!(old$url %in% new$url), , drop = FALSE]  # new wins on conflict
      combined <- dplyr::bind_rows(old, new)
    } else {
      added <- added + sum(!(new$url %in% relocated))
      combined <- new
    }

    combined <- combined[!duplicated(combined$url), , drop = FALSE]
    combined <- combined[order(combined$date, decreasing = TRUE), , drop = FALSE]
    saveRDS(combined, path, compress = "xz")
  }

  if (!quiet) {
    cli::cli_alert_success(
      "Archived {nrow(releases)} release(s) to {.path {dir}} ({added} new, {updated} updated)."
    )
  }
  invisible(list(dir = dir, added = added, updated = updated))
}

#' Read releases back from the on-disk archive
#'
#' @param dir Archive directory (see [archive_releases()]).
#' @param from,to Optional inclusive date bounds (`"YYYY-MM-DD"` or `Date`).
#'   Only the year partitions covering the range are loaded.
#' @return A [tibble][tibble::tibble] of releases (newest first), or a zero-row
#'   tibble if the archive is empty.
#' @seealso [archive_releases()], [download_archive()].
#' @export
read_archive <- function(dir = archive_dir(), from = NULL, to = NULL) {
  files <- archive_files(dir)
  if (length(files) == 0) {
    cli::cli_alert_info("No archive found in {.path {dir}}.")
    return(tibble::tibble())
  }
  from <- if (is.null(from)) NULL else as.Date(from)
  to <- if (is.null(to)) NULL else as.Date(to)

  # Skip year partitions wholly outside the requested range.
  yrs <- as.integer(sub("^.*releases-([0-9]{4})\\.rds$", "\\1", files))
  if (!is.null(from)) files <- files[yrs >= as.integer(format(from, "%Y"))]
  if (!is.null(to)) {
    yrs <- as.integer(sub("^.*releases-([0-9]{4})\\.rds$", "\\1", files))
    files <- files[yrs <= as.integer(format(to, "%Y"))]
  }
  if (length(files) == 0) return(tibble::tibble())

  out <- dplyr::bind_rows(lapply(files, readRDS))
  if (nrow(out) == 0) return(tibble::as_tibble(out))
  if (!is.null(from)) out <- out[out$date >= from, , drop = FALSE]
  if (!is.null(to)) out <- out[out$date <= to, , drop = FALSE]
  out <- out[order(out$date, decreasing = TRUE), , drop = FALSE]
  tibble::as_tibble(out)
}

#' Publish the archive as GitHub release assets
#'
#' Uploads each year partition to a GitHub release so others can fetch the
#' historical corpus without scraping. Uses the suggested \pkg{piggyback}
#' package; requires a `GITHUB_PAT` with write access to `repo`.
#'
#' @param dir Archive directory to publish.
#' @param repo GitHub repository (`"owner/name"`).
#' @param tag Release tag to attach the assets to (created if absent).
#' @param quiet Suppress progress messages.
#' @return Invisibly, the vector of uploaded file paths.
#' @seealso [download_archive()], [archive_releases()].
#' @export
publish_archive <- function(dir = archive_dir(), repo = "zaynesember/pressR",
                            tag = "archive", quiet = FALSE) {
  if (!requireNamespace("piggyback", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg piggyback} is required to publish archives.")
  }
  files <- archive_files(dir)
  if (length(files) == 0) cli::cli_abort("No archive partitions in {.path {dir}}.")

  # Ensure the release exists, using whichever creation API this piggyback
  # version exposes (1.x: pb_release_create; 0.1.x: pb_new_release). Only create
  # when the tag is absent, so re-publishing simply refreshes the assets.
  ns <- getNamespaceExports("piggyback")
  existing <- tryCatch(piggyback::pb_releases(repo = repo)$tag_name,
                       error = function(e) character(0))
  if (!tag %in% existing) {
    if ("pb_release_create" %in% ns) {
      piggyback::pb_release_create(repo = repo, tag = tag)
    } else if ("pb_new_release" %in% ns) {
      piggyback::pb_new_release(repo = repo, tag = tag)
    } else {
      cli::cli_abort("{.pkg piggyback} exposes neither {.fn pb_release_create} nor {.fn pb_new_release}.")
    }
  }
  for (f in files) {
    if (!quiet) cli::cli_alert_info("Uploading {.file {basename(f)}} to {repo}@{tag}.")
    piggyback::pb_upload(f, repo = repo, tag = tag)
  }
  invisible(files)
}

#' Download the published archive from GitHub release assets
#'
#' @inheritParams publish_archive
#' @param dir Directory to download the partitions into (defaults to the local
#'   archive dir, so [read_archive()] can read them afterward).
#' @return Invisibly, the archive directory.
#' @seealso [read_archive()], [publish_archive()].
#' @export
download_archive <- function(dir = archive_dir(), repo = "zaynesember/pressR",
                             tag = "archive", quiet = FALSE) {
  if (!requireNamespace("piggyback", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg piggyback} is required to download archives.")
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  piggyback::pb_download(repo = repo, tag = tag, dest = dir)
  if (!quiet) cli::cli_alert_success("Downloaded archive to {.path {dir}}.")
  invisible(dir)
}
