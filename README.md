# pressR

Automating the collection of U.S. Congress members' press releases.

`pressR` scrapes press releases from U.S. House (`*.house.gov`) and Senate
(`*.senate.gov`) members' websites over a date range and returns a tidy data
frame. Instead of guessing among dozens of CSS/XPath selectors per site, it
**detects the content-management system** behind each site and routes to a
dedicated extractor. Coverage is ~99% of the House and ~99% of the Senate.

## Why CMS detection

Member sites cluster into a few vendor families. The House skews to the
official Drupal template; the Senate skews to WordPress (often with a custom
press-release post type). Approximate House shares:

| CMS | Share | How it's scraped |
|-----|------:|------------------|
| **Drupal** (official House template) | ~54% | `/media/press-releases` listing, `?page=N` pagination |
| **ASP.NET** ("DocumentID") | ~20% | `documentquery.aspx` listing, `documentsingle.aspx` items |
| **WordPress** | ~12% (most of the Senate) | `wp-json` REST API — the `congress_press_release` category, a press-release custom post type, or an Elementor HTML fallback when REST is blocked |
| **press-releases vendor** | small | `/press-releases` (or `/media-center`) listing with `?ID=<GUID>` (year-less dates inferred) or `/YYYY/M/slug` item links; pager param auto-probed |
| **headless WordPress** (Next.js SPA) | small | public WPGraphQL endpoint (`/graphql`); releases at `/posts/<slug>` |
| **Other** | ~14% | generic heuristic extractor (heading-link or long-text titles + nearest date, incl. `/YYYY/M/slug` fallback) |

Five vendor extractors plus one generic fallback cover both chambers.
Pages are fetched statically by default; a headless-browser fallback
([`render_html()`], via the suggested **chromote** package) is available for the
rare JS-rendered site.

## Installation

```r
# install.packages("pak")
pak::pak("zaynesember/pressR")
```

## Usage

```r
library(pressR)

# Current members and their sites
members <- list_members()
#> # A tibble: 437 × 7
#>   name         state   district party committee           url                          chamber
#>   <chr>        <chr>   <chr>    <chr> <chr>               <chr>                        <chr>
#> 1 Moore, Barry Alabama 1st      R     Agriculture;Judici… https://barrymoore.house.gov house
#> ...

# One member
moore <- scrape_member("barrymoore.house.gov", from = "2026-01-01")

# A set of members (metadata carried through)
res <- scrape_pressers(
  members[1:10, ],
  from = "2026-01-01",
  to   = Sys.Date(),
  log_fails = TRUE          # writes fails.csv
)

# The whole House (optionally capped for a quick sample)
all <- scrape_house(from = "2026-01-01", max_members = 50)

# The Senate works the same way (same extractors, plus a `chamber` column)
senators <- list_senators()
sen <- scrape_senate(from = "2026-01-01", max_members = 50)
```

Every release-returning function yields columns `date`, `title`, `body`,
`tags`, `url`, `cms` (plus any member metadata). `scrape_pressers()` /
`scrape_house()` also attach a failures table:

```r
attr(res, "failures")   # tibble(url, stage, message)
```

## How it works

1. **`list_members()`** parses <https://www.house.gov/representatives>.
2. **`detect_cms()`** classifies a homepage via its `<meta generator>` tag and
   markup fingerprints.
3. The matching **extractor** finds the press-release listing, walks its pages
   newest-first until the window's start, and pulls each release's body.
4. **`scrape_member()`** ties these together for one site; **`scrape_pressers()`**
   runs many, isolating per-site failures.

## Politeness & configuration

Requests carry an identifying user agent and are throttled and retried via
[httr2](https://httr2.r-lib.org/). Tunable via options:

```r
options(pressR.throttle = 20)            # requests/minute (default 20)
options(pressR.cache_dir = "~/.cache/pressR")  # enable on-disk HTTP cache
```

## Archiving

Scrapes return in-memory tibbles; to build historical coverage, append runs to
a local, year-partitioned, de-duplicated store (xz-compressed RDS, keyed on
`url` so re-scrapes refresh rather than duplicate):

```r
res <- scrape_house(from = "2026-01-01")
archive_releases(res)                       # -> tools::R_user_dir("pressR","data")
read_archive(from = "2026-01-01", to = Sys.Date())
```

Set `options(pressR.archive_dir = "~/pressR-archive")` to choose the location.
Because the corpus grows (~33 MB/year compressed, body text included) it lives
on disk, not in the package. Prebuilt snapshots are published as GitHub release
assets; fetch them without scraping via `download_archive()` (and, for
maintainers with write access, `publish_archive()`). Both need the suggested
**piggyback** package.

## A look at the data

Archiving accumulates a tidy, one-row-per-release corpus across both chambers.
The published snapshot — fetch it with `download_archive()` — currently holds:

- **436,201 releases** from 537 member offices, **2010 through mid-2026**
- **~94%** carry full body text; issue tags wherever the CMS exposes them (~41% overall)
- **House** — 282,687 · **Senate** — 153,514 · by party ≈ 262k D · 173k R · 0.7k I

A reproducible recent slice (what `scrape_house(from = "2026-01-01")` yields):

```r
read_archive(from = "2026-01-01")
#> # A tibble: 24,122 × 12
#>   name             state party chamber date       title                                tags        cms
#>   <chr>            <chr> <chr> <chr>    <date>     <chr>                                <chr>       <chr>
#> 1 Latta, Robert    Ohio  R     house   2026-06-17 Latta Applauds FDA Approval of New … Veterans    aspx
#> 2 Hyde-Smith, Cindy MS   R     senate  2026-04-23 Hyde-Smith Backs Bill to Reauthoriz… Health Care drupal
#> 3 Clark, Katherine Mass… D     house   2026-06-18 Whip Clark Celebrates Reopening of … Health Care wordpress
#> # … plus district, committee, body, url
```

Every release is dated, attributed (member, state, party, chamber, committee),
and topic-tagged, so the corpus is ready for quick analysis:

```r
library(dplyr); library(tidyr)
a <- read_archive(from = "2026-01-01")

# Most-used issue tags
a |> filter(!is.na(tags)) |> separate_rows(tags, sep = ";") |> count(tags, sort = TRUE)
#>   tags            n
#>   Education    1198
#>   Veterans     1088
#>   Immigration   811
#>   Health Care   775
#>   Agriculture   584
#>   # …
```

Issue tags and state names are recorded as each chamber/office formats them, so
synonyms ("Health Care" vs "Healthcare") and mixed state forms (House full names,
Senate two-letter codes) appear — normalize before aggregating if needed.

## Development

```r
devtools::load_all()
devtools::test()     # offline unit tests run against saved HTML fixtures
devtools::check()
```

The original grad-school notebook implementation is preserved under
[`legacy/`](legacy/) for reference.

## Acknowledgements

Some of the original legacy code (under [`legacy/`](legacy/)) was generously
provided by Chris Stout.

## License

MIT © Zayne Sember
