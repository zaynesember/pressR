#' Scrape press releases for many members
#'
#' Runs [scrape_member()] across a set of member sites, isolating failures so
#' one broken site doesn't stop the run. Member metadata (name, state, etc.) is
#' carried through when `members` is a data frame such as that from
#' [list_members()].
#'
#' @param members Either a character vector of member URLs (optionally named,
#'   in which case names become the `name` column) or a data frame with a `url`
#'   column plus any metadata columns to carry through (e.g. from
#'   [list_members()]).
#' @param from,to Inclusive date range. `to` defaults to today.
#' @param fetch_bodies Whether to fetch each release's body text.
#' @param page_limit Per-member maximum listing pages to walk.
#' @param render Headless-browser policy passed to [scrape_member()]
#'   (`"auto"`, `"never"`, or `"always"`). For large batch runs `"never"`
#'   avoids spawning a browser on the unsupported tail; the default `"auto"`
#'   only renders sites that yield nothing statically (and only if
#'   \pkg{chromote} is installed).
#' @param retry_failed If `TRUE` (default), members that fail with an `"error"`
#'   (e.g. a listing that couldn't be located) are retried once at the end of
#'   the run, since such failures are often transient timeouts under load.
#'   `"empty"` results (listing found, nothing in range) are not retried.
#' @param log_fails If `TRUE`, write the failures table to `fails_path`.
#' @param fails_path Path for the failures CSV when `log_fails = TRUE`.
#' @param quiet Suppress per-member progress messages.
#' @return A [tibble][tibble::tibble] of releases (member metadata columns,
#'   then `date`, `title`, `body`, `tags`, `url`, `cms`). A `failures` tibble
#'   (`url`, `stage`, `message`) is attached as an attribute; retrieve it with
#'   `attr(result, "failures")`.
#' @seealso [scrape_house()] to scrape the whole chamber.
#' @export
scrape_pressers <- function(members, from, to = Sys.Date(),
                            fetch_bodies = TRUE, page_limit = 100,
                            render = c("auto", "never", "always"),
                            retry_failed = TRUE,
                            log_fails = FALSE, fails_path = "fails.csv",
                            quiet = FALSE) {
  render <- match.arg(render)
  from <- as.Date(from)
  to <- as.Date(to)
  if (from > to) cli::cli_abort("{.arg from} ({from}) is after {.arg to} ({to}).")

  meta <- normalize_members(members)
  n <- nrow(meta)
  if (n == 0) cli::cli_abort("No members to scrape.")

  # Scrape one member (by row index), returning the metadata-attached releases
  # tibble on success, or a list(stage, message) describing the failure.
  scrape_one <- function(i) {
    res <- tryCatch(
      scrape_member(meta$url[i], from = from, to = to, page_limit = page_limit,
                    fetch_bodies = fetch_bodies, render = render, quiet = TRUE),
      error = function(e) e
    )
    if (inherits(res, "error")) return(list(stage = "error", message = conditionMessage(res)))
    if (nrow(res) == 0) return(list(stage = "empty", message = "no releases in date range"))
    meta_cols <- setdiff(names(meta), c("url", names(res)))
    for (col in meta_cols) res[[col]] <- meta[[col]][i]
    res
  }

  results <- vector("list", n)
  stage <- rep(NA_character_, n)
  msg <- rep(NA_character_, n)

  # Interactive sessions get a progress bar with ETA; batch/log output keeps
  # one line per member so a detached run stays greppable.
  bar <- !quiet && interactive()
  n_rel <- 0L
  if (bar) {
    cli::cli_progress_bar(
      format = paste0("{cli::pb_bar} {cli::pb_current}/{cli::pb_total} members ",
                      "| {n_rel} releases | ETA {cli::pb_eta_str} | {cli::pb_status}"),
      total = n, clear = FALSE
    )
  }
  for (i in seq_len(n)) {
    if (bar) cli::cli_progress_update(set = i - 1L, status = sub("^https?://(www\\.)?", "", meta$url[i]))
    else if (!quiet) cli::cli_alert_info("[{i}/{n}] {meta$url[i]}")
    r <- scrape_one(i)
    if (is.data.frame(r)) { results[[i]] <- r; n_rel <- n_rel + nrow(r) } else { stage[i] <- r$stage; msg[i] <- r$message }
    if (bar) cli::cli_progress_update(set = i)
  }
  if (bar) cli::cli_progress_done()

  # Retry "error" failures once: these are often transient (a timeout during
  # listing discovery under load), not genuine. "empty" results are not retried
  # -- the listing was found, there are simply no releases in the window.
  retry_idx <- if (retry_failed) which(stage == "error") else integer(0)
  if (length(retry_idx) > 0) {
    if (!quiet) cli::cli_alert_info("Retrying {length(retry_idx)} failed member(s)...")
    for (i in retry_idx) {
      r <- scrape_one(i)
      if (is.data.frame(r)) {
        results[[i]] <- r; stage[i] <- NA_character_; msg[i] <- NA_character_
      } else {
        stage[i] <- r$stage; msg[i] <- r$message
      }
    }
  }

  out <- dplyr::bind_rows(purrr::compact(results))
  fail_idx <- which(!is.na(stage))
  failures <- tibble::tibble(
    url = meta$url[fail_idx], stage = stage[fail_idx], message = msg[fail_idx]
  )

  if (nrow(out) > 0) out <- order_columns(out)

  if (log_fails) {
    utils::write.csv(failures, fails_path, row.names = FALSE)
    cli::cli_alert_info("Logged {nrow(failures)} failure(s) to {.path {fails_path}}.")
  }

  if (!quiet) {
    cli::cli_alert_success(
      "Scraped {nrow(out)} release(s) from {n - nrow(failures)}/{n} member(s); {nrow(failures)} failure(s)."
    )
  }

  attr(out, "failures") <- failures
  out
}

#' Scrape press releases for the whole U.S. House
#'
#' Convenience wrapper that pulls the current member directory with
#' [list_members()] and scrapes it with [scrape_pressers()].
#'
#' @inheritParams scrape_pressers
#' @param max_members Optionally cap the number of members (e.g. for a quick
#'   sample); `NULL` scrapes everyone.
#' @return See [scrape_pressers()].
#' @export
scrape_house <- function(from, to = Sys.Date(), max_members = NULL,
                         fetch_bodies = TRUE, page_limit = 100,
                         render = c("auto", "never", "always"),
                         retry_failed = TRUE,
                         log_fails = FALSE, fails_path = "fails.csv",
                         quiet = FALSE) {
  render <- match.arg(render)
  members <- list_members()
  if (!is.null(max_members)) members <- utils::head(members, max_members)
  scrape_pressers(members, from = from, to = to, fetch_bodies = fetch_bodies,
                  page_limit = page_limit, render = render, retry_failed = retry_failed,
                  log_fails = log_fails, fails_path = fails_path, quiet = quiet)
}

#' Scrape press releases for the whole U.S. Senate
#'
#' Convenience wrapper that pulls the current senator directory with
#' [list_senators()] and scrapes it with [scrape_pressers()].
#'
#' @inheritParams scrape_pressers
#' @param max_members Optionally cap the number of senators (e.g. for a quick
#'   sample); `NULL` scrapes everyone.
#' @return See [scrape_pressers()].
#' @seealso [scrape_house()] for the House.
#' @export
scrape_senate <- function(from, to = Sys.Date(), max_members = NULL,
                          fetch_bodies = TRUE, page_limit = 100,
                          render = c("auto", "never", "always"),
                          retry_failed = TRUE,
                          log_fails = FALSE, fails_path = "fails.csv",
                          quiet = FALSE) {
  render <- match.arg(render)
  members <- list_senators()
  if (!is.null(max_members)) members <- utils::head(members, max_members)
  scrape_pressers(members, from = from, to = to, fetch_bodies = fetch_bodies,
                  page_limit = page_limit, render = render, retry_failed = retry_failed,
                  log_fails = log_fails, fails_path = fails_path, quiet = quiet)
}

# Coerce the `members` argument into a metadata tibble with a `url` column.
normalize_members <- function(members) {
  if (is.data.frame(members)) {
    if (!"url" %in% names(members)) cli::cli_abort("{.arg members} data frame needs a {.field url} column.")
    members$url <- normalize_home(members$url)
    return(tibble::as_tibble(members))
  }
  urls <- normalize_home(as.character(members))
  tibble::tibble(name = names(members) %||% rep(NA_character_, length(urls)), url = urls)
}

# Put member metadata first, then the release fields.
order_columns <- function(df) {
  release_cols <- c("date", "title", "body", "tags", "url", "cms")
  meta_cols <- setdiff(names(df), release_cols)
  dplyr::relocate(df, dplyr::all_of(meta_cols))
}
