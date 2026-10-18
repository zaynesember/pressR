# pressR

A dataset of U.S. congressional press releases, and the R package that builds
it by scraping every House and Senate member's website.

## The dataset

**Latest snapshot: October 18, 2026.** It holds 450,453 press releases from 540
members of Congress, January 2010 through October 18, 2026, and most include the
full text.

### Get it

In R, install the package and download the snapshot:

```r
# install.packages("pak")
pak::pak("zaynesember/pressR")
install.packages("piggyback")   # used by download_archive()

library(pressR)
download_archive()              # ~260 MB, one file per year (2010-2026)

releases <- read_archive()                          # everything (~1.5 GB in memory)
recent   <- read_archive(from = "2025-01-01")       # or just a date range
```

You can skip installing pressR: each year is a standalone `.rds` file on the
[`archive` release](https://github.com/zaynesember/pressR/releases/tag/archive),
which you can read with base R:

```r
url <- "https://github.com/zaynesember/pressR/releases/download/archive/releases-2025.rds"
download.file(url, "releases-2025.rds", mode = "wb")
r2025 <- readRDS("releases-2025.rds")
```

### What's in it

There is one row per press release:

| Column | Contents |
|--------|----------|
| `name` | Member, as `"Last, First"` |
| `state` | House: full state name. Senate: two-letter code |
| `district` | House district (e.g. `"9th"`); `NA` for senators |
| `party` | `D`, `R` or `I` |
| `chamber` | `house` or `senate` |
| `committee` | The member's committee assignments, separated by `;` |
| `date` | Release date |
| `title` | Headline |
| `body` | Full text (94% of rows; `NA` when the page was removed or is rendered by JavaScript) |
| `tags` | Issue tags as the office filed them, separated by `;` (41% of rows; many sites don't tag releases) |
| `url` | Link to the original release (unique per row) |
| `cms` | Which site type the release was scraped from |

As of the latest snapshot:

- **House:** 291,010 releases. **Senate:** 159,443.
- **By party:** 270,986 D, 178,634 R, 813 I.
- **Who's covered:** every member serving when the scrapes ran (the 119th
  Congress), plus a few who left office after a scrape. Former members who left
  before then aren't in it.
- **How far back:** each member's history goes back as far as their current
  website keeps it, so early years are thinner (see the table below).

<details>
<summary>Releases per year</summary>

| Year | House | Senate | Total |
|-----:|------:|-------:|------:|
| 2010 | 5,777 | 2,484 | 8,261 |
| 2011 | 7,158 | 3,407 | 10,565 |
| 2012 | 6,232 | 3,038 | 9,270 |
| 2013 | 9,785 | 3,498 | 13,283 |
| 2014 | 9,040 | 3,894 | 12,934 |
| 2015 | 12,069 | 5,820 | 17,889 |
| 2016 | 9,891 | 5,566 | 15,457 |
| 2017 | 16,489 | 8,426 | 24,915 |
| 2018 | 13,464 | 7,482 | 20,946 |
| 2019 | 19,043 | 8,935 | 27,978 |
| 2020 | 22,001 | 13,449 | 35,450 |
| 2021 | 26,171 | 14,399 | 40,570 |
| 2022 | 22,449 | 13,826 | 36,275 |
| 2023 | 26,924 | 16,118 | 43,042 |
| 2024 | 24,635 | 15,000 | 39,635 |
| 2025 | 35,045 | 19,571 | 54,616 |
| 2026 (to Oct 18) | 24,837 | 14,530 | 39,367 |

</details>

Issue tags and state names are recorded the way each office formats them, so
you'll see synonyms ("Health Care" vs "Healthcare") and both state forms.
Normalize them before aggregating.

## Working with the data

Every release is dated, attributed (member, state, party, chamber, committee),
and, where the office tags its releases, topic-tagged:

```r
library(dplyr); library(tidyr)
a <- read_archive(from = "2026-01-01")
#> # A tibble: 39,367 × 12
#>   name             state party chamber date       title                                tags        cms
#>   <chr>            <chr> <chr> <chr>    <date>     <chr>                                <chr>       <chr>
#> 1 Latta, Robert    Ohio  R     house   2026-06-17 Latta Applauds FDA Approval of New … Veterans    aspx
#> 2 Hyde-Smith, Cindy MS   R     senate  2026-04-23 Hyde-Smith Backs Bill to Reauthoriz… Health Care drupal
#> 3 Clark, Katherine Mass… D     house   2026-06-18 Whip Clark Celebrates Reopening of … Health Care wordpress
#> # … plus district, committee, body, url

# Most-used issue tags
a |> filter(!is.na(tags)) |> separate_rows(tags, sep = ";") |> count(tags, sort = TRUE)
#>   tags            n
#>   Education    2281
#>   Veterans     1960
#>   Immigration  1700
#>   Health Care  1639
#>   Congress     1491
#>   # …
```

## The scraper

`pressR` scrapes press releases from U.S. House (`*.house.gov`) and Senate
(`*.senate.gov`) members' websites over a date range and returns a tidy data
frame. Instead of guessing among dozens of CSS/XPath selectors per site, it
**detects the content-management system** behind each site and routes to a
dedicated extractor. Coverage is ~99% of the House and ~99% of the Senate.

### Why CMS detection

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

### Installation

```r
# install.packages("pak")
pak::pak("zaynesember/pressR")
```

### Usage

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

### How it works

1. **`list_members()`** parses <https://www.house.gov/representatives>.
2. **`detect_cms()`** classifies a homepage via its `<meta generator>` tag and
   markup fingerprints.
3. The matching **extractor** finds the press-release listing, walks its pages
   newest-first until the window's start, and pulls each release's body.
4. **`scrape_member()`** ties these together for one site; **`scrape_pressers()`**
   runs many, isolating per-site failures.

### Politeness & configuration

Requests carry an identifying user agent and are throttled and retried via
[httr2](https://httr2.r-lib.org/). Tunable via options:

```r
options(pressR.throttle = 20)            # requests/minute (default 20)
options(pressR.cache_dir = "~/.cache/pressR")  # enable on-disk HTTP cache
```

### Archiving

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
