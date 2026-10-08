# Build the DuckDB columnar store from the year-partitioned RDS archive.
# Streams one year at a time (never loads all bodies at once) and appends rows
# with derived metadata/flags. The store is the precompute substrate the
# downstream NLP and the Shiny dashboard read from (column/row pushdown means we
# can pull metadata-only slices without decompressing bodies).

suppressMessages({ library(DBI); library(duckdb) })

nlp_build_duckdb <- function(db_path = nlp_duckdb_path(), overwrite = TRUE) {
  if (overwrite && file.exists(db_path)) file.remove(db_path)
  con <- dbConnect(duckdb::duckdb(), dbdir = db_path)
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  total <- 0L
  nlp_stream_years(function(df, yr) {
    if (is.null(df$source)) df$source <- "scraped"
    df <- nlp_add_flags(df)
    # Stable column order; coerce date to character for duckdb DATE parsing.
    # `source` carries provenance (scraped / stout / wangtucker).
    keep <- c("url","date","title","name","state","district","party","committee",
              "chamber","cms","tags","body","source","body_nchar","body_ntokens",
              "has_body","is_stub","has_url","has_nav","is_spanish","usable")
    df <- df[, keep, drop = FALSE]
    df$date <- as.character(df$date)
    # type-stable VARCHAR across heterogeneous sources (scraped district may be chr)
    df$district <- as.character(df$district)
    df$tags <- as.character(df$tags)
    if (!dbExistsTable(con, "releases")) {
      dbWriteTable(con, "releases", df)
    } else {
      dbAppendTable(con, "releases", df)
    }
    total <<- total + nrow(df)
    invisible(NULL)
  })

  # URL uniqueness is an invariant downstream layers rely on -- quanteda uses
  # url as the docname and rbind2 REFUSES duplicate docnames, which aborted
  # tag-complete 14 minutes in. Duplicates are real: a member release published
  # on a minority-committee host is swept by both the member-site scrape and the
  # institutional collection at the identical URL (2 such urls in the first
  # combined build). Keep the first-inserted copy: the year archive streams
  # before external/, so the member-attributed `scraped` row wins and the
  # institutional copy is the one dropped.
  ndup <- dbExecute(con, "DELETE FROM releases WHERE rowid IN (
      SELECT rowid FROM (
        SELECT rowid, ROW_NUMBER() OVER (PARTITION BY url ORDER BY rowid) rn
        FROM releases) WHERE rn > 1)")
  if (ndup > 0) message(sprintf("  dropped %d duplicate-url row(s) (first-inserted copy kept)", ndup))

  # Type the date column + helpful indexes/views for fast dashboard slices.
  dbExecute(con, "ALTER TABLE releases ALTER date SET DATA TYPE DATE USING strptime(date,'%Y-%m-%d')::DATE")
  dbExecute(con, "ALTER TABLE releases ADD COLUMN year INTEGER")
  dbExecute(con, "UPDATE releases SET year = EXTRACT(year FROM date)")
  dbExecute(con, "CREATE INDEX idx_url ON releases(url)")
  dbExecute(con, "CREATE INDEX idx_year ON releases(year)")

  n <- dbGetQuery(con, "SELECT COUNT(*) n FROM releases")$n
  message(sprintf("DuckDB built: %s rows -> %s", format(n, big.mark=","), db_path))
  invisible(db_path)
}
