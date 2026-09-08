make_releases <- function(dates, urls, body = "x") {
  tibble::tibble(
    date = as.Date(dates),
    title = paste("t", seq_along(urls)),
    body = body,
    tags = NA_character_,
    url = urls,
    cms = "drupal"
  )
}

test_that("archive_releases writes year partitions that read_archive returns", {
  dir <- withr::local_tempdir()
  r <- make_releases(c("2026-03-01", "2025-11-15"), c("u/a", "u/b"))
  res <- archive_releases(r, dir = dir, quiet = TRUE)
  expect_equal(res$added, 2L)
  expect_setequal(
    basename(list.files(dir)),
    c("releases-2026.rds", "releases-2025.rds")
  )
  back <- read_archive(dir)
  expect_equal(nrow(back), 2L)
  expect_setequal(back$url, c("u/a", "u/b"))
  # newest first
  expect_true(back$date[1] >= back$date[nrow(back)])
})

test_that("archive_releases de-duplicates by url and lets a re-scrape update", {
  dir <- withr::local_tempdir()
  archive_releases(make_releases("2026-03-01", "u/a", body = "old"), dir = dir, quiet = TRUE)
  res <- archive_releases(make_releases("2026-03-01", "u/a", body = "new"), dir = dir, quiet = TRUE)
  expect_equal(res$added, 0L)
  expect_equal(res$updated, 1L)
  back <- read_archive(dir)
  expect_equal(nrow(back), 1L)          # not duplicated
  expect_equal(back$body, "new")        # refreshed
})

test_that("archive accumulates across runs", {
  dir <- withr::local_tempdir()
  archive_releases(make_releases("2026-01-10", "u/a"), dir = dir, quiet = TRUE)
  archive_releases(make_releases("2026-02-20", "u/b"), dir = dir, quiet = TRUE)
  expect_equal(nrow(read_archive(dir)), 2L)
})

test_that("read_archive filters by date range and loads only needed years", {
  dir <- withr::local_tempdir()
  archive_releases(
    make_releases(c("2024-06-01", "2025-06-01", "2026-06-01"), c("u/1", "u/2", "u/3")),
    dir = dir, quiet = TRUE
  )
  mid <- read_archive(dir, from = "2025-01-01", to = "2025-12-31")
  expect_equal(mid$url, "u/2")
  from_only <- read_archive(dir, from = "2025-06-01")
  expect_setequal(from_only$url, c("u/2", "u/3"))
})

test_that("archive_releases drops rows lacking a date or url", {
  dir <- withr::local_tempdir()
  r <- make_releases(c("2026-03-01", NA), c("u/a", "u/b"))
  r$url[1] <- NA  # also a missing url
  res <- archive_releases(r, dir = dir, quiet = TRUE)
  expect_equal(res$added, 0L)
})

test_that("read_archive on an empty dir returns a zero-row tibble", {
  dir <- withr::local_tempdir()
  out <- read_archive(dir)
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0L)
})

test_that("archive_releases resolves a url that moves to a different year", {
  # A source can reuse the same permalink for a genuinely different release
  # (an annual award's nominations re-opened at the same slug a year later),
  # or a re-scrape can simply resolve a date differently than before. Either
  # way the SAME url must never end up in two year files at once: quanteda
  # uses url as the docname, so a cross-file duplicate crashes the fold-in's
  # tag-complete step ("docnames must be unique") when the year files stream
  # together -- far away from, and much later than, the scrape that caused it.
  dir <- withr::local_tempdir()
  archive_releases(make_releases("2024-11-01", "u/a", body = "2024 announcement"),
                   dir = dir, quiet = TRUE)
  res <- archive_releases(make_releases("2026-09-01", "u/a", body = "2026 announcement"),
                          dir = dir, quiet = TRUE)
  expect_equal(res$added, 0L)
  expect_equal(res$updated, 1L)
  back <- read_archive(dir)
  expect_equal(nrow(back), 1L)                     # not duplicated across files
  expect_equal(back$body, "2026 announcement")     # new wins, same as in-year conflicts
  expect_false(file.exists(file.path(dir, "releases-2024.rds")) &&
               "u/a" %in% readRDS(file.path(dir, "releases-2024.rds"))$url)
})

test_that("archive_releases validates input", {
  expect_error(archive_releases(list(a = 1)), "data frame")
  expect_error(archive_releases(tibble::tibble(x = 1)), "date")
})
