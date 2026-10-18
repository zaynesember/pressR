#!/usr/bin/env Rscript
# Slice 5: member coordination network. Members are linked when they share a
# near-duplicate "message family" (joint statements, sign-on letters, reused
# talking points). v1 = SCRAPED source only (clean "Last, First" names, 2010-2026)
# to avoid cross-source name fragmentation; folding stout/wangtucker in via name
# canonicalization is a documented v2.
#   edge weight = sum over shared families of 1/(k-1)  (Newman co-membership
#   weighting, so a 30-member sign-on letter doesn't dominate); families with
#   >MAX_FAM members (mass sign-ons / procedural) are excluded from the dyadic
#   projection. Outputs: communities (Louvain), centrality, cross-party brokerage,
#   ideological homophily vs DW-NOMINATE, and a cross-party-coordination time series.
suppressMessages(devtools::load_all("/Users/zaynesember/GitRepos/pressR"))
source("/Users/zaynesember/GitRepos/pressR/nlp/R/00_foundation.R")
suppressMessages({ library(DBI); library(duckdb); library(data.table); library(igraph) })
t0 <- Sys.time(); OUT <- nlp_out_dir()
MAX_FAM <- 40L

# ---- name / state normalization for the DW-NOMINATE join --------------------
# Accents are transliterated and every non-letter dropped, so "SANCHEZ"/"SÁNCHEZ"
# and "O'Halleran"/"OHalleran" collapse to one form on both sides of the join.
ascii_lc <- function(x) {
  y <- suppressWarnings(iconv(tolower(trimws(x)), to = "ASCII//TRANSLIT"))
  ifelse(is.na(y), tolower(trimws(x)), y)
}
alpha_only <- function(x) gsub("[^a-z]", "", ascii_lc(x))
# surname minus generational suffix ("GONZALEZ, Vicente, Jr." -> "gonzalez")
norm_sur <- function(x)
  alpha_only(sub("\\s*,?\\s*\\b(jr|sr|ii|iii|iv|v)\\.?\\s*$", "", ascii_lc(sub(",.*$", "", x))))
# first given token, nickname stripped: "JOHNSON, Dustin (Dusty)" -> "dustin"
given_formal <- function(x) {
  g <- ascii_lc(sub("^[^,]*,\\s*", "", x))
  alpha_only(sub("\\s.*$", "", gsub("\\(.*|\".*", "", g)))
}
# the parenthesized/quoted nickname itself: "JOHNSON, Dustin (Dusty)" -> "dusty"
given_nick <- function(x) {
  g <- ascii_lc(sub("^[^,]*,\\s*", "", x))
  m <- regexpr("(?<=\\()[^)]+|(?<=\")[^\"]+", g, perl = TRUE)
  out <- rep(NA_character_, length(g)); ok <- m > 0
  out[ok] <- alpha_only(regmatches(g, m))
  out
}
# releases stores Senate rows as 2-letter abbrevs and House rows as full names
ST_ABB <- c(setNames(state.abb, state.name), "District of Columbia" = "DC",
            "Puerto Rico" = "PR", "Guam" = "GU", "American Samoa" = "AS",
            "Virgin Islands" = "VI", "Northern Mariana Islands" = "MP")
norm_state <- function(s) {
  s <- trimws(s)
  ifelse(is.na(s) | s == "", NA_character_,
         ifelse(nchar(s) == 2, toupper(s), unname(ST_ABB[s])))
}
# a congress convenes Jan 3 of 1789 + 2(N-1) and runs two years
congress_start <- function(n) as.Date(sprintf("%d-01-03", 1789 + 2 * (n - 1)))
congress_end   <- function(n) as.Date(sprintf("%d-01-03", 1789 + 2 * n))

## ---- pull scraped family memberships + member attributes ------------------
con <- dbConnect(duckdb::duckdb(), nlp_duckdb_path(), read_only = TRUE)
mem <- as.data.table(dbGetQuery(con, "
  SELECT rf.family_id, r.name
  FROM release_family rf JOIN releases r USING(url)
  WHERE r.source = 'scraped' AND r.usable AND r.name IS NOT NULL AND rf.is_reused"))
# state and the member's own publishing window join the roster here: both are
# needed to identify a member in Voteview (see the DW-NOMINATE block below).
matt <- as.data.table(dbGetQuery(con, "
  SELECT name, party, chamber, state, COUNT(*) n, MIN(date) d_lo, MAX(date) d_hi
  FROM releases
  WHERE source = 'scraped' AND name IS NOT NULL AND party IN ('D','R','I')
  GROUP BY 1,2,3,4"))
ts <- as.data.table(dbGetQuery(con, "
  SELECT CAST(EXTRACT(year FROM date_min) AS INT) yr,
    COUNT(*) FILTER (WHERE is_reused AND cross_member) n_coord,
    COUNT(*) FILTER (WHERE is_reused AND cross_member AND cross_party) n_xparty
  FROM families WHERE sources = 'scraped' AND date_min IS NOT NULL
  GROUP BY 1 HAVING CAST(EXTRACT(year FROM date_min) AS INT) BETWEEN 2010 AND 2026
  ORDER BY 1"))
# which issues draw cross-party coordination (a family is "on" each top_issue its
# releases touch; rate = share of coordinating families on that issue that are cross-party)
xp_issue <- as.data.table(dbGetQuery(con, "
  WITH fam AS (SELECT family_id, cross_party FROM families
               WHERE is_reused AND cross_member AND sources='scraped'),
       fi AS (SELECT DISTINCT f.family_id, f.cross_party, il.top_issue AS issue
              FROM fam f JOIN release_family rf USING(family_id) JOIN issue_labels il USING(url)
              WHERE il.top_issue IS NOT NULL)
  SELECT issue, COUNT(*) n_coord, SUM(cross_party::INT) n_xparty,
         ROUND(100.0*AVG(cross_party::INT),1) xparty_rate
  FROM fi GROUP BY 1 HAVING COUNT(*) >= 25 ORDER BY xparty_rate DESC"))
# cross-party rate by chamber scope (intra-House / intra-Senate / cross-chamber)
xp_chamber <- as.data.table(dbGetQuery(con, "
  WITH fam AS (SELECT family_id, cross_party FROM families
               WHERE is_reused AND cross_member AND sources='scraped'),
       fch AS (SELECT f.family_id, f.cross_party,
                 CASE WHEN COUNT(DISTINCT r.chamber)>1 THEN 'cross-chamber'
                      WHEN MAX(r.chamber)='house' THEN 'House-only' ELSE 'Senate-only' END AS \"scope\"
               FROM fam f JOIN release_family rf USING(family_id) JOIN releases r USING(url)
               GROUP BY 1,2)
  SELECT \"scope\", COUNT(*) n_coord, SUM(cross_party::INT) n_xparty,
         ROUND(100.0*AVG(cross_party::INT),1) xparty_rate FROM fch GROUP BY 1 ORDER BY 2 DESC"))
dbDisconnect(con, shutdown = TRUE)
ts[, xparty_share := round(n_xparty / n_coord, 3)]

# member-level modal party / chamber / state, plus the full publishing window
setorder(matt, name, -n)
mp <- matt[, .(party = party[1], chamber = chamber[1], state = state[1],
               d_lo = suppressWarnings(min(d_lo, na.rm = TRUE)),
               d_hi = suppressWarnings(max(d_hi, na.rm = TRUE))), by = name]
mp[!is.finite(d_lo), d_lo := NA]; mp[!is.finite(d_hi), d_hi := NA]

## ---- project families -> weighted member edges ----------------------------
mem <- unique(mem[, .(family_id, name)])
fk  <- mem[, .(k = .N), by = family_id]
mem <- merge(mem, fk, by = "family_id")[k >= 2 & k <= MAX_FAM]
pr  <- merge(mem, mem[, .(family_id, name)], by = "family_id",
             allow.cartesian = TRUE, suffixes = c("", "2"))[name < name2]
pr[, w := 1 / (k - 1)]
edges <- pr[, .(weight = sum(w), n_families = .N), by = .(a = name, b = name2)]
message(sprintf("  %s members in %s families -> %s edges",
  format(uniqueN(mem$name), big.mark=","), format(uniqueN(mem$family_id), big.mark=","),
  format(nrow(edges), big.mark=",")))

## ---- igraph metrics + communities -----------------------------------------
g <- graph_from_data_frame(edges[, .(a, b, weight, n_families)], directed = FALSE)
E(g)$dist <- 1 / E(g)$weight                      # strong tie = short path
V(g)$party   <- mp$party[match(V(g)$name, mp$name)]
V(g)$chamber <- mp$chamber[match(V(g)$name, mp$name)]
comm <- cluster_louvain(g, weights = E(g)$weight)
V(g)$community   <- as.integer(membership(comm))
V(g)$strength    <- strength(g, weights = E(g)$weight)
V(g)$degree      <- degree(g)
V(g)$betweenness <- betweenness(g, weights = E(g)$dist, directed = FALSE)
V(g)$eigen       <- eigen_centrality(g, weights = E(g)$weight)$vector

# per-edge + per-member cross-party coordination
el <- as.data.table(igraph::as_data_frame(g, "edges"))
el[, pa := mp$party[match(from, mp$name)]]
el[, pb := mp$party[match(to,   mp$name)]]
el[, cross_party := pa != pb & pa %in% c("D","R") & pb %in% c("D","R")]
cp <- rbind(el[, .(name = from, w = weight, cp = cross_party)],
            el[, .(name = to,   w = weight, cp = cross_party)])
cpm <- cp[, .(cross_party_strength = sum(w * cp), tot = sum(w)), by = name]
cpm[, cross_party_share := round(cross_party_strength / tot, 3)]

## ---- attach DW-NOMINATE ----------------------------------------------------
# This join used to key on lowercase "surname, firstword" against ALL 119
# congresses and take the highest congress number per key, with no era, state,
# chamber or party constraint. Where a key collided the modern member silently
# inherited a 19th-century namesake's score: "Johnson, Henry" (Hank Johnson,
# D-GA) took +0.272 from JOHNSON, Henry Underwood (R-IN, 55th Congress), and
# "McCormick, Richard" (R-GA) took +0.226 from McCORMICK, Richard Cunningham
# (54th) -- the same SIGN as his true +0.891, so a party-sign check could not
# catch it. Both failures trace to first-name FORM: Voteview writes "JOHNSON,
# Hank" and "MCCORMICK, Rich", so the formal roster name never reached the right
# person at all. The same mismatch silently NA'd 68 nodes ("JOHNSON, Dustin
# (Dusty)", "ROY, Charles", "REED, John F.").
#
# So: restrict to the corpus era, and key on surname + state + chamber, which
# does not depend on first-name form. Identity is BIOGUIDE_ID, not icpsr --
# Voteview mints a fresh 9xxxx icpsr for a party switcher (KILEY 22336->92336,
# VAN DREW 21980->91980) while bioguide is stable, and bioguide is the only
# thing separating the father/son pair MENENDEZ, Robert (M000639 b.1954 in the
# House through the 109th / M001226 b.1985 from the 118th) whom no name rule
# can tell apart. Residual ambiguity is broken by first name, then by overlap
# between the member's own publishing window and the office's service window.
# Anything still ambiguous is REPORTED and left unmatched, never guessed.
CORPUS_MIN_CONGRESS <- 109L                   # the 109th convened 2005-01-03
NOM_REPORT <- file.path(OUT, "network_nominate_unmatched.csv")

vv <- fread(file.path(OUT, "voteview_members.csv"))
vv <- vv[congress >= CORPUS_MIN_CONGRESS & chamber %in% c("House", "Senate") &
         !is.na(nominate_dim1) & !is.na(bioguide_id) & bioguide_id != ""]
stopifnot(nrow(vv) > 0)
vv[, `:=`(sur = norm_sur(bioname), formal = given_formal(bioname),
          nick = given_nick(bioname), st = toupper(trimws(state_abbrev)),
          ch = tolower(chamber),
          vv_party = fcase(party_code == 100, "D", party_code == 200, "R",
                           default = "I"))]
# One row per member-OFFICE (bioguide x chamber x state); a member who changed
# chamber holds two and the roster's own chamber selects between them. Within an
# office take the latest congress, breaking ties on vote count: Voteview carries
# a second thinly-voted row in a switch year (KILEY 119th: 386 votes as R, 107
# as I), and the well-voted row is the member's real position.
setorder(vv, bioguide_id, ch, st, congress, nominate_number_of_votes)
off <- vv[, .(nominate = nominate_dim1[.N], vv_party = vv_party[.N],
              src_congress = congress[.N], vv_bioname = bioname[.N],
              formal = formal[.N], nick = nick[.N],
              c_lo = min(congress), c_hi = max(congress)),
          by = .(bioguide = bioguide_id, sur, st, ch)]

ros <- mp[, .(name, party, chamber, state, d_lo, d_hi)]
ros[, `:=`(sur = norm_sur(name), giv = given_formal(name),
           st = norm_state(state), ch = tolower(chamber))]
if (anyNA(ros$st))
  warning("unmappable state(s) in roster: ",
          paste(unique(ros[is.na(st)]$state), collapse = ", "))

cand <- merge(ros, off, by = c("sur", "st", "ch"), allow.cartesian = TRUE)
# Narrow in two passes, each a no-op unless it actually discriminates. The
# survivor count after each pass is kept so `method` names the rule that really
# did the work: both MENENDEZ, Robert offices match the first name "robert",
# so crediting the first-name pass there would misreport how identity was
# settled (it was the service era).
cand[, n0 := .N, by = name]
cand[, fn_hit := (giv == formal) | (!is.na(nick) & giv == nick)]
cand[, keep := fn_hit | !any(fn_hit), by = name]; cand <- cand[keep == TRUE]
cand[, n1 := .N, by = name]
cand[, era_hit := !is.na(d_lo) & !is.na(d_hi) &
                  d_lo <= congress_end(c_hi) & d_hi >= congress_start(c_lo)]
cand[, keep := era_hit | !any(era_hit), by = name]; cand <- cand[keep == TRUE]
cand[, n_final := .N, by = name]
cand[, method := fcase(n0 == 1L, "surname+state+chamber",
                       n1 == 1L, "surname+state+chamber+firstname",
                       n_final == 1L, "surname+state+chamber+era",
                       default = "ambiguous")]

vnom  <- cand[n_final == 1L]
ambig <- cand[n_final > 1L]
nomatch <- ros[!name %in% cand$name]
# a matched row whose party contradicts the roster is a probable mis-identity
pmism <- vnom[party != vv_party]

# FAIL LOUDLY: the old join could not tell "no match" from "wrong match", so
# every unresolved or suspicious case is written out instead of becoming a
# silent NA in the nominate column.
prob <- rbind(
  nomatch[, .(name, party, state, chamber, issue = "no voteview candidate",
              detail = NA_character_)],
  ambig[, .(name, party, state, chamber, issue = "ambiguous",
            detail = paste(vv_bioname, bioguide, src_congress, sep = "/"))],
  pmism[, .(name, party, state, chamber, issue = "party mismatch",
            detail = paste0(vv_bioname, " is ", vv_party, " in voteview"))],
  fill = TRUE)
fwrite(prob, NOM_REPORT)
# full audit trail: every roster member, matched or not, with the identity the
# score came from. Covers roster members who are not network nodes (the
# MENENDEZ father/son pair among them), so the disambiguation rules stay
# inspectable even when they do not touch the graph.
fwrite(vnom[order(name), .(name, party, state, chamber, bioguide, vv_bioname,
                           vv_party, src_congress, nominate, method)],
       file.path(OUT, "network_nominate_matches.csv"))
stopifnot(all(vnom$src_congress >= CORPUS_MIN_CONGRESS))
if (nrow(ambig))
  warning(sprintf("%d roster name(s) matched >1 person and were left unmatched: %s",
                  uniqueN(ambig$name), paste(unique(ambig$name), collapse = "; ")))
if (nrow(pmism))
  warning(sprintf("%d matched member(s) disagree with the roster on party: %s",
                  nrow(pmism), paste(pmism$name, collapse = "; ")))

nodes <- data.table(
  name = V(g)$name, party = V(g)$party, chamber = V(g)$chamber,
  community = V(g)$community, degree = V(g)$degree,
  strength = round(V(g)$strength, 2), betweenness = round(V(g)$betweenness, 1),
  eigen = round(V(g)$eigen, 3))
nodes <- merge(nodes, cpm[, .(name, cross_party_strength = round(cross_party_strength, 2),
                              cross_party_share)], by = "name", all.x = TRUE)
# provenance travels with the score so a downstream consumer can audit it
# (and see immediately that every source congress is inside the corpus era)
nodes <- merge(nodes, vnom[, .(name, nominate, nominate_bioguide = bioguide,
                               nominate_congress = src_congress,
                               nominate_bioname = vv_bioname,
                               nominate_match = method)],
               by = "name", all.x = TRUE)

## ---- summary stats --------------------------------------------------------
nomv <- nodes$nominate[match(V(g)$name, nodes$name)]
assort_party <- assortativity_nominal(g, as.integer(factor(V(g)$party)), directed = FALSE)
# ideological homophily: assortativity() can't take NAs -> compute on the matched subgraph
matched <- which(!is.na(nomv))
assort_nom <- assortativity(igraph::induced_subgraph(g, matched), nomv[matched], directed = FALSE)
summ <- data.table(
  n_nodes = vcount(g), n_edges = ecount(g),
  n_communities = max(nodes$community),
  modularity = round(modularity(comm), 3),
  assort_party = round(assort_party, 3),       # +1 = party-homophilous, 0 = random
  assort_nominate = round(assort_nom, 3),
  pct_edges_crossparty = round(100 * mean(el$cross_party), 1),
  nominate_matched = sum(!is.na(nodes$nominate)))

## ---- persist (duckdb tables + rds) ----------------------------------------
edges_out <- el[, .(a = from, b = to, weight = round(weight, 3), n_families,
                    cross_party)]
con <- dbConnect(duckdb::duckdb(), nlp_duckdb_path())
dbWriteTable(con, "network_nodes", as.data.frame(nodes),     overwrite = TRUE)
dbWriteTable(con, "network_edges", as.data.frame(edges_out), overwrite = TRUE)
dbWriteTable(con, "network_ts",    as.data.frame(ts),        overwrite = TRUE)
dbWriteTable(con, "network_summary", as.data.frame(summ),    overwrite = TRUE)
dbWriteTable(con, "network_xparty_issue",   as.data.frame(xp_issue),   overwrite = TRUE)
dbWriteTable(con, "network_xparty_chamber", as.data.frame(xp_chamber), overwrite = TRUE)
dbDisconnect(con, shutdown = TRUE)
saveRDS(list(nodes = nodes, edges = edges_out, ts = ts, summary = summ,
             xparty_issue = xp_issue, xparty_chamber = xp_chamber),
        file.path(OUT, "network.rds"))

cat("\n================ COORDINATION NETWORK (scraped v1) ================\n")
print(summ)
cat("\nTop 15 cross-party brokers (most coordination weight reaching the other party):\n")
brk <- nodes[party %in% c("D","R")][order(-cross_party_strength)][1:15,
  .(name, party, chamber, community, betweenness, cross_party_strength, cross_party_share,
    nominate)]
print(brk)
cat("\nCommunity sizes (top 8) + party mix:\n")
cm <- nodes[, .(n = .N, D = sum(party=="D"), R = sum(party=="R"), I = sum(party=="I")),
            by = community][order(-n)][1:8]
print(cm)
cat("\n---- DW-NOMINATE join ----\n")
cat(sprintf("nodes %d | matched %d | unmatched %d\n", nrow(nodes),
            sum(!is.na(nodes$nominate)), sum(is.na(nodes$nominate))))
cat("match method:\n"); print(table(nodes$nominate_match, useNA = "ifany"))
cat(sprintf("source congress range: %d-%d (corpus era starts at %d)\n",
            min(nodes$nominate_congress, na.rm = TRUE),
            max(nodes$nominate_congress, na.rm = TRUE), CORPUS_MIN_CONGRESS))
# sign anomalies are legitimate but must never be silent: print them so any
# D-with-positive-score in a downstream figure is a known, inspectable row
anom <- nodes[(party == "D" & nominate > 0) | (party == "R" & nominate < 0),
              .(name, party, chamber, nominate, nominate_bioname, nominate_congress)]
cat(sprintf("party/sign anomalies: %d\n", nrow(anom)))
if (nrow(anom)) print(anom)
if (nrow(prob)) { cat(sprintf("unresolved/suspicious (see %s):\n", basename(NOM_REPORT)))
                  print(prob[, .N, by = issue]) }

cat("\nCross-party rate by chamber scope:\n"); print(xp_chamber)
cat("\nMost bipartisan issues (highest cross-party coordination rate):\n")
print(head(xp_issue, 8)); cat("Least bipartisan:\n"); print(tail(xp_issue, 6))
cat(sprintf("\nartifacts: network.rds, duckdb network_nodes/edges/ts/summary/xparty_issue/xparty_chamber\n"))
cat(sprintf("done in %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
