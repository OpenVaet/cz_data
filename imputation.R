# step1_offsets.R
# R 4.2+ recommended
suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
})

eurostat_deaths_file <- "data/demo_r_mwk_05_linear_2_0.csv"
mzcr_file            <- "data/mzcr_no_or_first_infection.csv"

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

# ---------------------------
# Helpers
# ---------------------------

age_bins <- data.table(
  lower = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90),
  upper = c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, 999),
  label = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
            "40-44","45-49","50-54","55-59","60-64","65-69","70-74",
            "75-79","80-84","85-89","90-999")
)

age_labels <- age_bins$label
all_age_groups <- age_labels
both_sexes <- c("M","F")

# Vectorized age→group (for MZCR)
age_to_group <- function(x) {
  cut(as.integer(x),
      breaks = c(-Inf, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, Inf),
      labels = age_labels, right = TRUE)
}

# Eurostat age label → group
map_euro_age_group <- function(v) {
  fcase(
    v == "Y_LT5: Less than 5 years",          "0-4",
    v == "Y5-9: From 5 to 9 years",            "5-9",
    v == "Y10-14: From 10 to 14 years",        "10-14",
    v == "Y15-19: From 15 to 19 years",        "15-19",
    v == "Y20-24: From 20 to 24 years",        "20-24",
    v == "Y25-29: From 25 to 29 years",        "25-29",
    v == "Y30-34: From 30 to 34 years",        "30-34",
    v == "Y35-39: From 35 to 39 years",        "35-39",
    v == "Y40-44: From 40 to 44 years",        "40-44",
    v == "Y45-49: From 45 to 49 years",        "45-49",
    v == "Y50-54: From 50 to 54 years",        "50-54",
    v == "Y55-59: From 55 to 59 years",        "55-59",
    v == "Y60-64: From 60 to 64 years",        "60-64",
    v == "Y65-69: From 65 to 69 years",        "65-69",
    v == "Y70-74: From 70 to 74 years",        "70-74",
    v == "Y75-79: From 75 to 79 years",        "75-79",
    v == "Y80-84: From 80 to 84 years",        "80-84",
    v == "Y85-89: From 85 to 89 years",        "85-89",
    v == "Y_GE90: 90 years or over",           "90-999",
    default = NA_character_
  )
}

# ---------------------------
# Load Eurostat (fast & robust)
# ---------------------------
euro <- fread(eurostat_deaths_file, sep = "auto", encoding = "UTF-8", showProgress = TRUE)

# Standardize column names we need
req_cols_euro <- c("sex: Sex",
                   "geo: Geopolitical entity (reporting)",
                   "OBS_VALUE: Observation value",
                   "TIME_PERIOD: Time",
                   "age: Age class")
stopifnot(all(req_cols_euro %in% names(euro)))

setnames(euro, old = req_cols_euro,
               new = c("sex", "geo", "obs_value", "time_period", "age"))

# Filter to Czechia + exclude totals/unknowns; keep only M/F
euro <- euro[
  geo == "CZ: Czechia" &
  sex %chin% c("F: Females","M: Males") &
  !age %chin% c("TOTAL: Total","UNK: Unknown")
]

# Map sex to single-letter, age to group, split year/week
euro[, sex := ifelse(sex == "F: Females", "F", "M")]
euro[, age_group := map_euro_age_group(age)]
if (any(is.na(euro$age_group))) {
  stop("Unexpected Eurostat age label encountered.")
}

# TIME_PERIOD looks like "YYYY-Www" (sometimes "YYYY-WW")
euro[, year := as.integer(sub("-.*", "", time_period))]
euro[, week := as.integer(sub(".*-W?", "", time_period))]

# Sum to year/week/age_group/sex
euro_agg <- euro[, .(eurostat_deaths = sum(as.numeric(obs_value), na.rm = TRUE)),
                 by = .(year, week, age_group, sex)]

# ---------------------------
# Load MZCR (12M+ rows)
# ---------------------------
mz <- fread(mzcr_file, sep = "auto", encoding = "UTF-8", showProgress = TRUE)

req_cols_mz <- c("week_date_of_death","year_of_birth_end","age_at_death","gender","ID")
stopifnot(all(req_cols_mz %in% names(mz)))

# Coerce gender robustly before filtering/deriving sex
mz[, gender := suppressWarnings(as.integer(gender))]

# Keep rows where week_date_of_death AND year_of_birth_end are present
mz_known <- mz[
  nzchar(week_date_of_death) & !is.na(week_date_of_death) &
  nzchar(as.character(year_of_birth_end)) & !is.na(year_of_birth_end)
]

# Map sex (1 -> M, 2 -> F, else U)
mz_known[, sex := fcase(
  gender == 1L, "M",
  gender == 2L, "F",
  default = "U"
)]

# ISO week/year from week_date_of_death
mz_known[, death_date := as.IDate(week_date_of_death)]
mz_known[, week := isoweek(death_date)]
mz_known[, year := isoyear(death_date)]

# Age group (numeric age_at_death)
mz_known[, age_at_death := as.integer(age_at_death)]
mz_known[, age_group := age_to_group(age_at_death)]

# Increment counts (note: 'U' is kept here, but offsets will consider only M/F)
mz_agg <- mz_known[, .N, by = .(year, week, age_group, sex)]
setnames(mz_agg, "N", "mzcr_deaths")

# ---------------------------
# Write imputation layer 1
# ---------------------------
fwrite(
  mz_known[, .(id = ID,
               sex,
               year_of_birth_end,
               week_date_of_death,
               age_at_death)],
  file = "outputs/imputation_layer_1.csv",
  quote = FALSE
)

# ---------------------------
# Build the deaths offset table
#   - Iterate over (year,week) present in MZCR
#   - Skip (year==2020 & week<10) and all of 2024
#   - For each age_group x sex in {M,F}, compare MZCR vs Eurostat
# ---------------------------
weeks_in_mz <- unique(mz_agg[, .(year, week)])
weeks_in_mz <- weeks_in_mz[!(year == 2020 & week < 10) & year != 2024]

# Full grid of (year,week) × age_groups × sex(M/F)
# Expand over observed (year,week) pairs
full <- weeks_in_mz[, .(year, week)
  ][, CJ(age_group = all_age_groups, sex = both_sexes, unique = TRUE)
  , by = .(year, week)]
setcolorder(full, c("year","week","age_group","sex"))

# Attach counts (left join; missing→0)
full <- merge(full, euro_agg, by = c("year","week","age_group","sex"), all.x = TRUE)
full <- merge(full, mz_agg[sex %chin% both_sexes],
              by = c("year","week","age_group","sex"), all.x = TRUE)

for (col in c("eurostat_deaths","mzcr_deaths")) {
  full[is.na(get(col)), (col) := 0L]
}

full[, mzcr_minus_eurostats := mzcr_deaths - eurostat_deaths]

# ---------------------------
# Write offsets CSV
# ---------------------------
setcolorder(full, c("year","week","age_group","sex","mzcr_deaths","eurostat_deaths","mzcr_minus_eurostats"))
setkey(full, year, week, age_group, sex)
fwrite(full, file = "outputs/deaths_offset.csv", quote = FALSE)

# ---------------------------
# Recap totals
# ---------------------------
total_deaths_extra   <- full[mzcr_minus_eurostats > 0, sum(mzcr_minus_eurostats)]
total_deaths_missing <- full[mzcr_minus_eurostats <= 0, sum(mzcr_minus_eurostats)]

cat(strrep("-", 50), "\n", sep = "")
cat("----- Pre-imputation Recap. -----\n")
cat(strrep("-", 50), "\n", sep = "")
cat(sprintf("total_deaths_extra       : %d\n", abs(total_deaths_extra)))
cat(sprintf("total_deaths_missing     : %d\n", total_deaths_missing))
cat(strrep("-", 50), "\n", sep = "")

# ---------------------------
# Step 2 — Re-attribution from control CSVs + per-person imputation
# ---------------------------

stopifnot(file.exists("outputs/deaths_offset.csv"),
          file.exists("outputs/imputation_layer_1.csv"))

age_levels <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                "35-39","40-44","45-49","50-54","55-59","60-64",
                "65-69","70-74","75-79","80-84","85-89","90-999")
sexes <- c("M","F")

parse_group <- function(g) {
  p <- as.integer(strsplit(g, "-", fixed = TRUE)[[1]])
  list(from = p[1], to = p[2])
}

# -- Read offsets
deaths_dt <- fread("outputs/deaths_offset.csv", encoding = "UTF-8")
deaths_dt[, `:=`(
  year  = as.integer(year),
  week  = as.integer(week),
  sex   = as.character(sex),
  age_group = as.character(age_group),
  eurostat_deaths = as.integer(eurostat_deaths),
  mzcr_deaths     = as.integer(mzcr_deaths),
  mzcr_minus_eurostats = as.integer(mzcr_minus_eurostats)
)]
setkey(deaths_dt, year, week, age_group, sex)

# Recap
total_deaths_extra   <- deaths_dt[mzcr_minus_eurostats > 0, sum(mzcr_minus_eurostats, na.rm = TRUE)]
total_deaths_missing <- deaths_dt[mzcr_minus_eurostats <= 0, sum(mzcr_minus_eurostats, na.rm = TRUE)]
cat(sprintf(
  "[%d] too many deaths in MZCR in a given sex & age group, aren't in Eurostat - and must be re-attributed to the nearest age group (%d deaths from Eurostat aren't in MZCR as well).\n",
  abs(total_deaths_extra), total_deaths_missing
))

# Container for planned re-attributions
attribs <- data.table(
  year = integer(), week = integer(), age_group_from = character(),
  sex = character(), age_group_to = character(), to_reattrib = integer()
)

# Small helpers
years <- sort(unique(deaths_dt$year))

get_off <- function(y, w, g, s) {
  v <- deaths_dt[list(y, w, g, s), mzcr_minus_eurostats]
  if (!length(v)) NA_real_ else as.numeric(v)
}

do_reattrib <- function(y, w, s, from_group, to_group, amount) {
  if (amount <= 0) return(invisible(NULL))
  ii_from <- deaths_dt[list(y, w, from_group, s), which = TRUE]
  ii_to   <- deaths_dt[list(y, w, to_group,   s), which = TRUE]
  if (!length(ii_from) || !length(ii_to)) return(invisible(NULL))

  deaths_dt[ii_from, mzcr_deaths := mzcr_deaths - amount]
  deaths_dt[ii_from, mzcr_minus_eurostats := mzcr_deaths - eurostat_deaths]

  deaths_dt[ii_to,   mzcr_deaths := mzcr_deaths + amount]
  deaths_dt[ii_to,   mzcr_minus_eurostats := mzcr_deaths - eurostat_deaths]

  attribs <<- rbind(
    attribs,
    data.table(year = y, week = w, age_group_from = from_group,
               sex = s, age_group_to = to_group, to_reattrib = as.integer(amount)),
    use.names = TRUE
  )
}

# ---- 0-4 → 5-9
for (y in years) {
  if (y == 2024) next
  weeks_y <- sort(unique(deaths_dt[year == y, week]))
  for (w in weeks_y) {
    if (y == 2020 && w < 10) next
    for (s in sexes) {
      cur_off  <- get_off(y, w, "0-4", s)
      next_off <- get_off(y, w, "5-9", s)
      if (isTRUE(cur_off > 0) && isTRUE(next_off < 0)) {
        amt <- as.integer(min(cur_off, abs(next_off)))
        if (amt > 0) do_reattrib(y, w, s, "0-4", "5-9", amt)
      }
    }
  }
}

# ---- Middle groups: 5-9 .. 85-89 (prefer NEXT, then PREV if still remaining)
mid_groups <- age_levels[age_levels != "0-4" & age_levels != "90-999"]

for (grp in mid_groups) {
  b <- parse_group(grp)
  next_grp <- sprintf("%d-%d", b$from + 5, b$to + 5)
  prev_grp <- sprintf("%d-%d", b$from - 5, b$to - 5)

  for (y in years) {
    if (y == 2024) next
    weeks_y <- sort(unique(deaths_dt[year == y, week]))
    for (w in weeks_y) {
      if (y == 2020 && w < 10) next
      for (s in sexes) {
        cur_off <- get_off(y, w, grp, s)
        if (is.na(cur_off) || cur_off <= 0) next

        # Try NEXT neighbor first
        next_off <- get_off(y, w, next_grp, s)
        if (isTRUE(next_off < 0)) {
          amt1 <- as.integer(min(cur_off, abs(next_off)))
          if (amt1 > 0) do_reattrib(y, w, s, grp, next_grp, amt1)
        }

        # If still positive, try PREV neighbor
        cur_off2 <- get_off(y, w, grp, s)
        if (is.na(cur_off2) || cur_off2 <= 0) next
        prev_off <- get_off(y, w, prev_grp, s)
        if (isTRUE(prev_off < 0)) {
          amt2 <- as.integer(min(cur_off2, abs(prev_off)))
          if (amt2 > 0) do_reattrib(y, w, s, grp, prev_grp, amt2)
        }
      }
    }
  }
}

# ---- 90-999 → 85-89
for (y in years) {
  if (y == 2024) next
  weeks_y <- sort(unique(deaths_dt[year == y, week]))
  for (w in weeks_y) {
    if (y == 2020 && w < 10) next
    for (s in sexes) {
      cur_off  <- get_off(y, w, "90-999", s)
      prev_off <- get_off(y, w, "85-89",  s)
      if (isTRUE(cur_off > 0) && isTRUE(prev_off < 0)) {
        amt <- as.integer(min(cur_off, abs(prev_off)))
        if (amt > 0) do_reattrib(y, w, s, "90-999", "85-89", amt)
      }
    }
  }
}

# Collapse/aggregate re-attributions
attribs_agg <- if (nrow(attribs)) {
  attribs[, .(to_reattrib = sum(to_reattrib)), by = .(year, week, age_group_from, sex, age_group_to)]
} else attribs
setorder(attribs_agg, year, week, sex, age_group_from, age_group_to)

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)
fwrite(attribs_agg, "outputs/imputation_layer_2_attributions.csv", quote = FALSE)

# -------------------------------------
# Apply per-person imputation (layer 1)
# -------------------------------------

# Read the layer 1 CSV
mz2 <- fread("outputs/imputation_layer_1.csv", encoding = "UTF-8")
mz2[, `:=`(
  id                = suppressWarnings(as.integer(id)),
  sex               = as.character(sex),
  year_of_birth_end = suppressWarnings(as.integer(year_of_birth_end)),
  week_date_of_death= as.character(week_date_of_death),
  age_at_death      = suppressWarnings(as.integer(age_at_death))
)]
mz2[, death_date := as.IDate(week_date_of_death)]
mz2[, `:=`(death_year = isoyear(death_date), death_week = isoweek(death_date))]
mz2[, age_group := as.character(cut(
  as.integer(age_at_death),
  breaks = c(-Inf, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, Inf),
  labels = age_levels,
  right  = TRUE
))]
mz2[, age_group := as.character(cut(
  as.integer(age_at_death),
  breaks = c(-Inf, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, Inf),
  labels = age_levels,
  right  = TRUE
))]
mz2[, age_group_from := age_group]   # <-- add this line

# Build a mutable "remaining" map to consume per person
attribs_remain <- copy(attribs_agg)
setkey(attribs_remain, sex, year, week, age_group_from, age_group_to)

reattributed <- 0L
detail_rows <- vector("list", 0)

# Iterate rows in file order (one step to an adjacent bin)
usable <- mz2[!is.na(year_of_birth_end) & !is.na(death_date) &
              sex %chin% sexes & !is.na(age_group_from)]
if (nrow(usable)) {
  idxs <- usable[, .I]
  for (i in idxs) {
    r <- mz2[i]
    cand <- attribs_remain[.(r$sex, r$death_year, r$death_week, r$age_group_from)]
    if (!nrow(cand)) next
    cand <- cand[!is.na(to_reattrib) & to_reattrib > 0L]
    if (!nrow(cand)) next

    # iterates sorted keys; pick first destination lexicographically
    setorder(cand, age_group_to)
    picked <- cand$age_group_to[1L]

    # Step ±5 toward target then clamp into target bounds
    cur_bounds <- parse_group(r$age_group_from)
    tgt_bounds <- parse_group(picked)
    proposed   <- r$age_at_death + if (tgt_bounds$from > cur_bounds$from) 5L else -5L
    new_age    <- min(max(proposed, tgt_bounds$from), tgt_bounds$to)

    if (!is.finite(new_age) || is.na(new_age) || new_age == r$age_at_death) next

    delta_age  <- new_age - r$age_at_death
    yob_before <- r$year_of_birth_end
    age_before <- r$age_at_death

    # Update subject
    mz2[i, `:=`(
      age_at_death      = as.integer(new_age),
      year_of_birth_end = as.integer(yob_before - delta_age)
    )]

    # Decrement one planned move on this (edge); drop if depleted
    attribs_remain[.(r$sex, r$death_year, r$death_week, r$age_group_from, picked),
                   to_reattrib := to_reattrib - 1L]
    attribs_remain <- attribs_remain[!is.na(to_reattrib) & to_reattrib > 0L]
    setkey(attribs_remain, sex, year, week, age_group_from, age_group_to)

    # Log detail
    detail_rows[[length(detail_rows) + 1L]] <- data.table(
      id = r$id,
      death_year = r$death_year,
      death_week = r$death_week,
      sex = r$sex,
      year_of_birth_end = yob_before,
      week_date_of_death = r$week_date_of_death,
      age_at_death = age_before,
      imputed_year_of_birth = yob_before - delta_age,
      imputed_age = new_age
    )
    reattributed <- reattributed + 1L
  }
}

# Write outputs
fwrite(
  mz2[, .(id, sex, year_of_birth_end, week_date_of_death, age_at_death)],
  "outputs/imputation_layer_2.csv",
  quote = FALSE
)

details_dt <- if (length(detail_rows)) rbindlist(detail_rows, use.names = TRUE) else
  data.table(id=integer(), death_year=integer(), death_week=integer(), sex=character(),
             year_of_birth_end=integer(), week_date_of_death=character(),
             age_at_death=integer(), imputed_year_of_birth=integer(), imputed_age=integer())
setcolorder(details_dt, c("id","death_year","death_week","sex","year_of_birth_end",
                          "week_date_of_death","age_at_death","imputed_year_of_birth","imputed_age"))
fwrite(details_dt, "outputs/imputation_layer_2_details.csv", quote = FALSE)

# Recap
planned <- if (nrow(attribs_agg)) attribs_agg[, sum(to_reattrib)] else 0L
cat(strrep("-", 50), "\n", sep = "")
cat(sprintf("Planned reattributions : %d\n", planned))
cat(sprintf("Applied  reattributions: %d\n", reattributed))
cat(strrep("-", 50), "\n", sep = "")

# (Optional) show most unmet edges if any remain
if (reattributed != planned) {
  pool <- mz2[!is.na(year_of_birth_end) & !is.na(death_date) &
              sex %chin% sexes &
              !is.na(age_group_from),
              .N, by = .(sex, death_year, death_week, age_group_from)]
  setnames(pool, "N", "available")

  short <- merge(
    attribs_agg,
    pool,
    by.x = c("sex","year","week","age_group_from"),
    by.y = c("sex","death_year","death_week","age_group_from"),
    all.x = TRUE
  )
  short[is.na(available), available := 0L]
  short[, unmet := pmax(to_reattrib - available, 0L)]
  print(short[unmet > 0][order(-unmet)][1:20])
}


# ---------------------------
# Step 3 — Weekly YOB/age imputation using Eurostat shares
# ---------------------------

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
})

eurostat_deaths_file <- "data/demo_r_mwk_05_linear_2_0.csv"
mzcr_step2_file      <- "outputs/imputation_layer_2.csv"
mzcr_origin_file     <- "data/mzcr_no_or_first_infection.csv"

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

age_levels <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                "35-39","40-44","45-49","50-54","55-59","60-64",
                "65-69","70-74","75-79","80-84","85-89","90-999")
sexes <- c("M","F")

# ---------- helpers ----------
parse_group <- function(g) {
  p <- as.integer(strsplit(g, "-", fixed = TRUE)[[1]])
  list(from = p[1], to = p[2])
}
# Replace any previous definition
age_group_of_age <- function(a) {
  a <- as.integer(a)
  as.character(cut(
    a,
    breaks = c(-Inf, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, Inf),
    labels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
               "35-39","40-44","45-49","50-54","55-59","60-64",
               "65-69","70-74","75-79","80-84","85-89","90-999"),
    right = TRUE
  ))
}

# ---- define %||% BEFORE it's used anywhere ----
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

# ---- Largest remainder with caps, using column name `bin` (NOT `key`) ----
apportion_with_caps <- function(total, items_df) {
  # items_df: data.table with columns bin, weight, cap
  alloc <- integer(nrow(items_df)); names(alloc) <- items_df$bin
  if (total <= 0L || nrow(items_df) == 0L) return(alloc)

  rows <- items_df[cap > 0L]
  if (nrow(rows) == 0L) return(alloc)

  sum_w <- sum(rows$weight)
  if (sum_w <= 0) {
    rem <- total
    for (i in seq_len(nrow(rows))) {
      if (rem <= 0) break
      take <- min(rows$cap[i], rem)
      alloc[rows$bin[i]] <- take
      rem <- rem - take
    }
    return(alloc)
  }

  rows[, quota  := total * (weight / sum_w)]
  rows[, floor_q := pmin(cap, floor(quota))]
  allocated <- sum(rows$floor_q)
  remain <- total - allocated
  rows[, remainder := quota - floor_q]

  if (remain > 0) {
    ord <- order(rows$remainder, decreasing = TRUE)
    for (i in ord) {
      if (remain <= 0) break
      if (rows$floor_q[i] < rows$cap[i]) {
        rows$floor_q[i] <- rows$floor_q[i] + 1L
        remain <- remain - 1L
      }
    }
  } else if (remain < 0) {
    ord <- order(rows$remainder, decreasing = FALSE)
    for (i in ord) {
      if (remain >= 0) break
      if (rows$floor_q[i] > 0L) {
        rows$floor_q[i] <- rows$floor_q[i] - 1L
        remain <- remain + 1L
      }
    }
  }

  alloc[rows$bin] <- rows$floor_q
  alloc
}

# ---------- 1) Load Eurostat deaths: year/week/age_group/sex ----------
euro <- fread(eurostat_deaths_file, sep = "auto", encoding = "UTF-8", showProgress = FALSE)
setnames(euro,
         old = c("sex: Sex","geo: Geopolitical entity (reporting)","OBS_VALUE: Observation value",
                 "TIME_PERIOD: Time","age: Age class"),
         new = c("sex","geo","obs_value","time_period","age"))
euro <- euro[geo == "CZ: Czechia" &
               sex %chin% c("F: Females","M: Males") &
               !age %chin% c("TOTAL: Total","UNK: Unknown")]

# map Eurostat age to our bins
map_euro_age_group <- function(v) fcase(
  v == "Y_LT5: Less than 5 years",          "0-4",
  v == "Y5-9: From 5 to 9 years",            "5-9",
  v == "Y10-14: From 10 to 14 years",        "10-14",
  v == "Y15-19: From 15 to 19 years",        "15-19",
  v == "Y20-24: From 20 to 24 years",        "20-24",
  v == "Y25-29: From 25 to 29 years",        "25-29",
  v == "Y30-34: From 30 to 34 years",        "30-34",
  v == "Y35-39: From 35 to 39 years",        "35-39",
  v == "Y40-44: From 40 to 44 years",        "40-44",
  v == "Y45-49: From 45 to 49 years",        "45-49",
  v == "Y50-54: From 50 to 54 years",        "50-54",
  v == "Y55-59: From 55 to 59 years",        "55-59",
  v == "Y60-64: From 60 to 64 years",        "60-64",
  v == "Y65-69: From 65 to 69 years",        "65-69",
  v == "Y70-74: From 70 to 74 years",        "70-74",
  v == "Y75-79: From 75 to 79 years",        "75-79",
  v == "Y80-84: From 80 to 84 years",        "80-84",
  v == "Y85-89: From 85 to 89 years",        "85-89",
  v == "Y_GE90: 90 years or over",           "90-999",
  default = NA_character_
)
euro[, age_group := map_euro_age_group(age)]
euro[, sex := ifelse(sex == "F: Females", "F", "M")]
euro[, year := as.integer(sub("-.*","", time_period))]
euro[, week := as.integer(sub(".*-W?", "", time_period))]
euro_agg <- euro[, .(eurostat_deaths = sum(as.integer(obs_value), na.rm = TRUE)),
                 by = .(year, week, age_group, sex)]

# ---------- 2) Load reimputed (known YOB) deaths from Step 2 ----------
# Build (year,week,age_group,sex) counts AND keep per-ID fields for output
mz2 <- fread(mzcr_step2_file, encoding = "UTF-8")
mz2[, `:=`(
  id                = suppressWarnings(as.integer(id)),
  sex               = as.character(sex),
  year_of_birth_end = suppressWarnings(as.integer(year_of_birth_end)),
  week_date_of_death= as.character(week_date_of_death),
  age_at_death      = suppressWarnings(as.integer(age_at_death))
)]
mz2[, death_date := as.IDate(week_date_of_death)]
mz2[, `:=`(death_year = isoyear(death_date), death_week = isoweek(death_date))]
mz2[, age_group := age_group_of_age(age_at_death)]

# filter time window for offsets
valid_mask <- !(mz2$death_year == 2020 & mz2$death_week < 10) & mz2$death_year != 2024
mz2_valid <- mz2[valid_mask]

mzcr_counts <- mz2_valid[sex %chin% sexes & !is.na(age_group),
                         .N, by = .(death_year, death_week, age_group, sex)]
setnames(mzcr_counts, c("death_year","death_week","N"),
         c("year","week","mzcr_deaths"))

# ---------- Compare to Eurostat to find deficits; write deaths_offset_2.csv ----------
full_keys <- CJ(year = unique(mz2_valid$death_year),
                week = unique(mz2_valid$death_week),
                age_group = age_levels,
                sex = sexes, unique = TRUE)
deaths2 <- merge(full_keys, euro_agg, by = c("year","week","age_group","sex"), all.x = TRUE)
deaths2 <- merge(deaths2, mzcr_counts, by = c("year","week","age_group","sex"), all.x = TRUE)
deaths2[is.na(eurostat_deaths), eurostat_deaths := 0L]
deaths2[is.na(mzcr_deaths),     mzcr_deaths     := 0L]
deaths2[, mzcr_minus_eurostats := mzcr_deaths - eurostat_deaths]

# apply week filters on the output, too
deaths2_out <- deaths2[!(year == 2020 & week < 10) & year != 2024]
setorder(deaths2_out, year, week, age_group, sex)
fwrite(deaths2_out,
       "outputs/deaths_offset_2.csv",
       quote = FALSE)

# Build "weeks requiring imputation" (only the negative offsets)
weeks_req <- deaths2_out[mzcr_minus_eurostats < 0,
                         .(missing_in_mzcr = as.integer(abs(mzcr_minus_eurostats)),
                           eurostat_deaths),
                         by = .(year, week, age_group, sex)]

# ---------- 3) Scan the original MZCR file to get totals and list of IDs with no YOB ----------
orig <- fread(mzcr_origin_file, sep = "auto", encoding = "UTF-8", showProgress = FALSE)
orig[, `:=`(
  ID                 = suppressWarnings(as.integer(ID)),
  week_date_of_death = as.character(week_date_of_death),
  year_of_birth_end  = suppressWarnings(as.integer(year_of_birth_end)),
  gender             = suppressWarnings(as.integer(gender))
)]
orig[, sex := fcase(
  gender == 1L, "M",
  gender == 2L, "F",
  default = "U"
)]
orig[, death_date := as.IDate(week_date_of_death)]
orig <- orig[!is.na(death_date)]
orig[, `:=`(death_year = isoyear(death_date), death_week = isoweek(death_date))]

# Totals per (year,week,sex), and count with missing YOB
totals <- orig[, .(
  total_deaths = .N,
  total_deaths_with_no_dob = sum(is.na(year_of_birth_end))
), by = .(death_year, death_week, sex)]
setnames(totals, c("death_year","death_week"),
         c("year","week"))

# List of IDs with no YOB
no_yob_ids <- orig[is.na(year_of_birth_end),
                   .(id = ID, sex, week_date_of_death, year = death_year, week = death_week)]

# Convenience lookups
total_unknown_per_week <- dcast(totals, year + week ~ sex,
                                value.var = "total_deaths_with_no_dob", fill = 0L)

# ---------- 4) Weekly planning: Stage A (same-sex pool) + Stage B (unknown-sex pool) ----------
# Build a nested list for optional debugging/CSV
weekly <- new.env(parent = emptyenv())

# helper to set/get nested members easily
ensure_week <- function(y, w) {
  key <- sprintf("%d-%02d", y, w)
  if (!exists(key, weekly, inherits = FALSE)) {
    weekly[[key]] <- list(
      weekly_total_eurostat_deaths = 0L,
      weekly_total_known_yob_mzcr_deaths = 0L,
      weekly_total_to_attribute = 0L,
      weekly_unknown_sex_unknown_available = 0L,
      weekly_total_same_sex_unknown_available = 0L,
      weekly_overall_unknown_yob_available = 0L,
      age_groups = new.env(parent = emptyenv())
    )
  }
  weekly[[key]]
}
ensure_ag_sx <- function(W, ag, sx) {
  agenv <- W$age_groups
  if (!exists(ag, agenv, inherits = FALSE)) {
    agenv[[ag]] <- new.env(parent = emptyenv())
  }
  node <- agenv[[ag]]
  if (is.null(node[[sx]])) {
    node[[sx]] <- list(
      missing_in_mzcr = 0L,
      eurostat_deaths = 0L,
      unknown_sex_to_attrib = 0L,
      same_sex_unknown_available = 0L,
      eurostat_age_group_share_of_total_deaths = 0,
      deaths_to_attrib_on_ag_sex = 0L
    )
  }
  node[[sx]]
}

# ---------- Iterate weeks that require imputation (no `key` symbol anywhere) ----------
need_weeks <- unique(weeks_req[, .(year, week)])
setorder(need_weeks, year, week)

for (rw in seq_len(nrow(need_weeks))) {
  y <- need_weeks$year[rw]; w <- need_weeks$week[rw]
  W <- ensure_week(y, w)

  # Availability from original file: unknown YOB counts by sex
  avail_row <- total_unknown_per_week[year == y & week == w]
  avail_M <- if (nrow(avail_row)) avail_row$M else 0L
  avail_F <- if (nrow(avail_row)) avail_row$F else 0L
  avail_U <- if (nrow(avail_row)) avail_row$U else 0L
  if (is.na(avail_M)) avail_M <- 0L
  if (is.na(avail_F)) avail_F <- 0L
  if (is.na(avail_U)) avail_U <- 0L

  # Needs and weights
  bins <- weeks_req[year == y & week == w]
  bins[, bin := paste(age_group, sex, sep = "|")]               # <-- renamed
  bin_need   <- setNames(as.integer(bins$missing_in_mzcr), bins$bin)
  bin_weight <- setNames(as.integer(bins$eurostat_deaths), bins$bin)
  bin_need[is.na(bin_need)]     <- 0L
  bin_weight[is.na(bin_weight)] <- 0L

  weekly_total_to_attribute <- sum(bin_need)
  weekly_total_euro <- sum(bin_weight)

  # Fill weekly header fields
  W$weekly_total_eurostat_deaths <- as.integer(weekly_total_euro)
  W$weekly_total_known_yob_mzcr_deaths <- 0L
  W$weekly_total_to_attribute <- as.integer(weekly_total_to_attribute)
  W$weekly_unknown_sex_unknown_available <- as.integer(avail_U)
  W$weekly_total_same_sex_unknown_available <- as.integer(avail_M + avail_F)
  W$weekly_overall_unknown_yob_available <- as.integer(avail_U + avail_M + avail_F)

  # Stage A: same-sex pools (M then F), with caps = needs
  for (sx in sexes) {
    avail <- if (sx == "M") avail_M else avail_F
    if (avail <= 0) next

    # items: bins ending with |sx
    k_sx <- grep(paste0("\\|", sx, "$"), names(bin_need), value = TRUE)
    if (length(k_sx)) {
      items <- data.table(
        bin    = k_sx,
        weight = bin_weight[k_sx],
        cap    = bin_need[k_sx]
      )
      items[is.na(weight), weight := 0L]
      items[is.na(cap),    cap    := 0L]

      alloc <- apportion_with_caps(avail, items)
      # apply
      for (bn in names(alloc)) {
        x <- as.integer(alloc[[bn]]); if (x <= 0L) next
        bin_need[bn] <- as.integer(bin_need[bn] - x)
        parts <- strsplit(bn, "\\|", fixed = FALSE)[[1]]
        ag <- parts[1]; s <- parts[2]
        node <- ensure_ag_sx(W, ag, s)
        node$same_sex_unknown_available <- as.integer(node$same_sex_unknown_available + x)
        node$missing_in_mzcr  <- as.integer((node$missing_in_mzcr %||% 0L) + 0L)
        node$eurostat_deaths  <- as.integer(node$eurostat_deaths %||% (bin_weight[bn] %||% 0L))
      }
    }
  }

  # Stage B: unknown-sex pool (U)
  if (avail_U > 0L) {
    items <- data.table(
      bin    = names(bin_need),
      weight = unname(bin_weight),
      cap    = unname(bin_need)
    )
    items[is.na(weight), weight := 0L]
    items[is.na(cap),    cap    := 0L]
    items <- items[cap > 0]
    if (nrow(items)) {
      alloc <- apportion_with_caps(avail_U, items)
      for (bn in names(alloc)) {
        x <- as.integer(alloc[[bn]]); if (x <= 0L) next
        bin_need[bn] <- as.integer(bin_need[bn] - x)
        parts <- strsplit(bn, "\\|", fixed = FALSE)[[1]]
        ag <- parts[1]; s <- parts[2]
        node <- ensure_ag_sx(W, ag, s)
        node$unknown_sex_to_attrib <- as.integer(node$unknown_sex_to_attrib + x)
        node$missing_in_mzcr  <- as.integer((node$missing_in_mzcr %||% 0L) + 0L)
        node$eurostat_deaths  <- as.integer(node$eurostat_deaths %||% (bin_weight[bn] %||% 0L))
      }
    }
  }

  # Ensure baseline fields for all bins in this week
  for (bn in names(bin_need)) {
    parts <- strsplit(bn, "\\|", fixed = FALSE)[[1]]
    ag <- parts[1]; s <- parts[2]
    base_need <- weeks_req[year == y & week == w & age_group == ag & sex == s,
                           missing_in_mzcr][1]
    base_eu   <- weeks_req[year == y & week == w & age_group == ag & sex == s,
                           eurostat_deaths][1]
    node <- ensure_ag_sx(W, ag, s)
    node$missing_in_mzcr <- as.integer(base_need %||% 0L)
    node$eurostat_deaths <- as.integer(base_eu   %||% 0L)
    if ((W$weekly_total_eurostat_deaths %||% 0L) > 0L) {
      node$eurostat_age_group_share_of_total_deaths <-
        (node$eurostat_deaths / W$weekly_total_eurostat_deaths)
    } else {
      node$eurostat_age_group_share_of_total_deaths <- 0
    }
  }
}

# ---------- 5) Output planning details ----------
plan_out <- "outputs/layer_3_deaths_yob_imputation_by_by_ages_and_sexes.csv"
f <- file(plan_out, open = "w+", encoding = "UTF-8")
writeLines(paste(
  "year","week","age_group","sex",
  "weekly_total_eurostat_deaths",
  "weekly_total_known_yob_mzcr_deaths",
  "weekly_total_to_attribute",
  "weekly_unknown_sex_unknown_available",
  "weekly_available_pct",
  "weekly_total_same_sex_unknown_available",
  "weekly_overall_unknown_yob_available",
  "missing_in_mzcr",
  "eurostat_deaths",
  "eurostat_age_group_share_of_total_deaths",
  "deaths_to_attrib_on_ag_sex",
  "unknown_sex_to_attrib",
  "same_sex_unknown_available",
  sep = ","
), con = f)

emit_row <- function(y, w, ag, s, W, node) {
  line <- paste(
    y, w, ag, s,
    W$weekly_total_eurostat_deaths %||% 0L,
    W$weekly_total_known_yob_mzcr_deaths %||% 0L,
    W$weekly_total_to_attribute %||% 0L,
    W$weekly_unknown_sex_unknown_available %||% 0L,
    0,
    W$weekly_total_same_sex_unknown_available %||% 0L,
    W$weekly_overall_unknown_yob_available %||% 0L,
    node$missing_in_mzcr %||% 0L,
    node$eurostat_deaths %||% 0L,
    node$eurostat_age_group_share_of_total_deaths %||% 0,
    node$deaths_to_attrib_on_ag_sex %||% 0L,
    node$unknown_sex_to_attrib %||% 0L,
    node$same_sex_unknown_available %||% 0L,
    sep = ","
  )
  writeLines(line, con = f)
}

wk_keys <- ls(weekly)
wk_keys <- wk_keys[order(as.integer(sub("-.*","", wk_keys)),
                         as.integer(sub(".*-","", wk_keys)))]
for (key in wk_keys) {
  parts <- strsplit(key, "-", fixed = TRUE)[[1]]
  y <- as.integer(parts[1]); w <- as.integer(parts[2])
  W <- weekly[[key]]
  ags <- ls(W$age_groups)
  # sort by lower bound then upper bound
  ags <- ags[order(as.integer(sub("-.*","", ags)),
                   as.integer(sub(".*-","", ags)))]
  for (ag in ags) {
    node_env <- W$age_groups[[ag]]
    for (s in sort(names(as.list(node_env)))) {
      node <- node_env[[s]]
      emit_row(y, w, ag, s, W, node)
    }
  }
}
close(f)
cat("Wrote", plan_out, "\n")

# ---------- 6) Output known YOB deaths to layer 3 ----------
out3 <- "outputs/imputation_layer_3.csv"
con3 <- file(out3, open = "w+", encoding = "UTF-8")
writeLines("id,sex,year_of_birth_end,week_date_of_death,age_at_death,death_year,death_week,age_group", con3)

# Print known (already have YOB)
setorder(mz2, id, na.last = TRUE)
n_known <- 0L
for (i in seq_len(nrow(mz2))) {
  r <- mz2[i]
  if (is.na(r$year_of_birth_end) || is.na(r$death_date)) next
  ag <- age_group_of_age(r$age_at_death)
  line <- paste(r$id, r$sex, r$year_of_birth_end, r$week_date_of_death, r$age_at_death,
                r$death_year, r$death_week, ag, sep = ",")
  writeLines(line, con = con3)
  n_known <- n_known + 1L
}

# ---------- 7) Stage A/B per-ID use of planned bins; collect leftovers ----------
# Build quick index of no-YOB IDs by (year,week,sex) preserving input order
setorder(no_yob_ids, id)
leftovers <- new.env(parent = emptyenv())
push_leftover <- function(y, w, sx, id) {
  key <- sprintf("%d-%02d-%s", y, w, sx)
  if (is.null(leftovers[[key]])) leftovers[[key]] <- integer()
  leftovers[[key]] <- c(leftovers[[key]], id)
}

get_week_node <- function(y, w) {
  key <- sprintf("%d-%02d", y, w)
  if (!exists(key, weekly, inherits = FALSE)) return(NULL)
  weekly[[key]]
}

n_total_unknown <- nrow(no_yob_ids)
n_non_attributed <- 0L
n_stageAB <- 0L

for (i in seq_len(n_total_unknown)) {
  r <- no_yob_ids[i]
  y <- r$year; w <- r$week; sx <- r$sex
  W <- get_week_node(y, w)
  if (is.null(W)) {  # week not in imputation set
    push_leftover(y, w, sx, r$id)
    n_non_attributed <- n_non_attributed + 1L
    next
  }

  ag_chosen <- NA_character_
  if (sx %chin% sexes) {
    # same-sex pool
    ags <- ls(W$age_groups)
    ags <- ags[order(as.integer(sub("-.*","", ags)), as.integer(sub(".*-","", ags)))]
    picked <- FALSE
    for (ag in ags) {
      node <- W$age_groups[[ag]][[sx]]
      if (!is.null(node) && (node$same_sex_unknown_available %||% 0L) > 0L) {
        node$same_sex_unknown_available <- node$same_sex_unknown_available - 1L
        W$age_groups[[ag]][[sx]] <- node
        ag_chosen <- ag
        picked <- TRUE
        break
      }
    }
    if (!picked) {
      push_leftover(y, w, sx, r$id)
      n_non_attributed <- n_non_attributed + 1L
      next
    }
  } else {
    # Unknown sex: use unknown_sex_to_attrib across bins (and set sx to bin's sex)
    ags <- ls(W$age_groups)
    ags <- ags[order(as.integer(sub("-.*","", ags)), as.integer(sub(".*-","", ags)))]
    picked <- FALSE
    for (ag in ags) {
      node_env <- W$age_groups[[ag]]
      for (sx2 in names(as.list(node_env))) {
        node <- node_env[[sx2]]
        if (!is.null(node) && (node$unknown_sex_to_attrib %||% 0L) > 0L) {
          node$unknown_sex_to_attrib <- node$unknown_sex_to_attrib - 1L
          node_env[[sx2]] <- node
          ag_chosen <- ag
          sx <- sx2
          picked <- TRUE
          break
        }
      }
      if (picked) break
    }
    if (!picked) {
      push_leftover(y, w, r$sex, r$id)
      n_non_attributed <- n_non_attributed + 1L
      next
    }
  }

  # Emit row for chosen bin: YOB = death_year - lower_bound; age = lower_bound
  from <- parse_group(ag_chosen)$from
  yob_end <- y - from
  age_at_death <- from
  line <- paste(r$id, sx, yob_end, r$week_date_of_death, age_at_death, y, w, ag_chosen, sep = ",")
  writeLines(line, con = con3)
  n_stageAB <- n_stageAB + 1L
}

# ---------- 8) Stage C: Eurostat redistribution for leftovers ----------
split_key <- function(key) {
  m <- regexec("^([0-9]+)-([0-9]+)-([MFU])$", key)
  p <- regmatches(key, m)[[1]]
  list(year = as.integer(p[2]), week = as.integer(p[3]), sex = p[4])
}

# Quick access to Eurostat totals per (year,week,sex,age_group)
get_euro_wt <- function(y, w, sx) {
  euro_agg[year == y & week == w & sex == sx, .(age_group, eurostat_deaths)]
}

left_keys <- ls(leftovers)
n_redistributed <- 0L

for (key in left_keys) {
  parts <- strsplit(key, "-", fixed = TRUE)[[1]]
  y <- as.integer(parts[1]); w <- as.integer(parts[2]); sx <- parts[3]
  ids <- leftovers[[key]]

  if (sx == "U") {
    N <- length(ids); if (N == 0) next
    sumM <- euro_agg[year == y & week == w & sex == "M", sum(eurostat_deaths)]
    sumF <- euro_agg[year == y & week == w & sex == "F", sum(eurostat_deaths)]

    # use 'bin' not 'key' to avoid data.table's key= arg
    items <- data.table(
      bin    = c("M","F"),
      weight = c(ifelse(sumM > 0, sumM, 1),
                 ifelse(sumF > 0, sumF, 1)),
      cap    = N
    )
    split_alloc <- apportion_with_caps(N, items)
    to_M <- as.integer(split_alloc["M"] %||% 0L)
    to_F <- as.integer(split_alloc["F"] %||% 0L)

    ids_M <- if (to_M > 0) ids[seq_len(min(to_M, length(ids)))] else integer()
    ids   <- ids[-seq_along(ids_M)]
    ids_F <- if (to_F > 0) ids[seq_len(min(to_F, length(ids)))] else integer()
    ids   <- ids[-seq_along(ids_F)]

    while (length(ids) > 0) {
      if (length(ids_M) <= length(ids_F)) { ids_M <- c(ids_M, ids[1]); ids <- ids[-1]
      } else { ids_F <- c(ids_F, ids[1]); ids <- ids[-1] }
    }

    for (sx2 in c("M","F")) {
      L  <- if (sx2 == "M") ids_M else ids_F
      N2 <- length(L); if (N2 == 0) next

      wt <- get_euro_wt(y, w, sx2)
      if (nrow(wt) == 0 || sum(wt$eurostat_deaths) <= 0) {
        wt <- data.table(age_group = age_levels, eurostat_deaths = 1L)
      } else {
        wt <- merge(data.table(age_group = age_levels), wt, by = "age_group", all.x = TRUE)
        wt[is.na(eurostat_deaths), eurostat_deaths := 0L]
      }

      items2 <- data.table(bin = wt$age_group, weight = wt$eurostat_deaths, cap = N2)
      alloc  <- apportion_with_caps(N2, items2)

      for (ag in names(alloc)) {
        take <- as.integer(alloc[[ag]]); if (take <= 0) next
        if (take > length(L)) take <- length(L)
        ids_take <- L[seq_len(take)]
        L <- L[-seq_len(take)]

        from <- parse_group(ag)$from
        yob_end <- y - from
        age_at_death <- from
        for (idv in ids_take) {
          wdd <- no_yob_ids[id == idv, week_date_of_death][1]
          line <- paste(idv, sx2, yob_end, wdd, age_at_death, y, w, ag, sep = ",")
          writeLines(line, con = con3)
          n_redistributed <- n_redistributed + 1L
        }
      }
    }

  } else {
    # Known sex leftovers: allocate by Eurostat age shares
    N <- length(ids); if (N == 0) next
    wt <- get_euro_wt(y, w, sx)
    if (nrow(wt) == 0 || sum(wt$eurostat_deaths) <= 0) {
      wt <- data.table(age_group = age_levels, eurostat_deaths = 1L)
    } else {
      wt <- merge(data.table(age_group = age_levels), wt, by = "age_group", all.x = TRUE)
      wt[is.na(eurostat_deaths), eurostat_deaths := 0L]
    }

    items <- data.table(bin = wt$age_group, weight = wt$eurostat_deaths, cap = N)
    alloc <- apportion_with_caps(N, items)

    for (ag in names(alloc)) {
      take <- as.integer(alloc[[ag]]); if (take <= 0) next
      if (take > length(ids)) take <- length(ids)
      ids_take <- ids[seq_len(take)]
      ids <- ids[-seq_len(take)]

      from <- parse_group(ag)$from
      yob_end <- y - from
      age_at_death <- from
      for (idv in ids_take) {
        wdd <- no_yob_ids[id == idv, week_date_of_death][1]
        line <- paste(idv, sx, yob_end, wdd, age_at_death, y, w, ag, sep = ",")
        writeLines(line, con = con3)
        n_redistributed <- n_redistributed + 1L
      }
    }
  }
}

close(con3)

cat(strrep("-", 50), "\n", sep = "")
cat(sprintf("Known YOB printed                 : %d\n", n_known))
cat(sprintf("Attributed (Stage A/B)            : %d\n", n_stageAB))
cat(sprintf("Redistributed by Eurostat (Stage C): %d\n", n_redistributed))
cat(sprintf("Total written to layer 3          : %d\n", n_known + n_stageAB + n_redistributed))
cat(strrep("-", 50), "\n", sep = "")

# ---------------------------
# Step 4 — Population YOB imputation to match Eurostat
# ---------------------------

suppressPackageStartupMessages({
  library(data.table)
})

eurostat_pop_file <- "data/demo_pjan_linear_2_0.csv"
mzcr_origin_file  <- "data/mzcr_no_or_first_infection.csv"

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

age_levels <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                "35-39","40-44","45-49","50-54","55-59","60-64",
                "65-69","70-74","75-79","80-84","85-89","90-999")
sexes <- c("M","F")

# ---------- helpers ----------
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

parse_group <- function(g) {
  p <- as.integer(strsplit(g, "-", fixed = TRUE)[[1]])
  list(from = p[1], to = p[2])
}

# age integer -> 5y bin label
age_group_of_age <- function(a) {
  a <- as.integer(a)
  as.character(cut(
    a,
    breaks = c(-Inf, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, Inf),
    labels = age_levels, right = TRUE
  ))
}

# Largest remainder with caps: items_df must have columns bin, weight, cap
apportion_with_caps <- function(total, items_df) {
  alloc <- integer(nrow(items_df)); names(alloc) <- items_df$bin
  if (total <= 0L || nrow(items_df) == 0L) return(alloc)

  rows <- items_df[cap > 0L]
  if (nrow(rows) == 0L) return(alloc)

  sum_w <- sum(rows$weight)
  if (sum_w <= 0) {
    rem <- total
    for (i in seq_len(nrow(rows))) {
      if (rem <= 0) break
      take <- min(rows$cap[i], rem)
      alloc[rows$bin[i]] <- take
      rem <- rem - take
    }
    return(alloc)
  }

  rows[, quota   := total * (weight / sum_w)]
  rows[, floor_q := pmin(cap, floor(quota))]
  allocated <- sum(rows$floor_q)
  remain <- total - allocated
  rows[, remainder := quota - floor_q]

  if (remain > 0) {
    ord <- order(rows$remainder, decreasing = TRUE)
    for (i in ord) {
      if (remain <= 0) break
      if (rows$floor_q[i] < rows$cap[i]) {
        rows$floor_q[i] <- rows$floor_q[i] + 1L
        remain <- remain - 1L
      }
    }
  } else if (remain < 0) {
    ord <- order(rows$remainder, decreasing = FALSE)
    for (i in ord) {
      if (remain >= 0) break
      if (rows$floor_q[i] > 0L) {
        rows$floor_q[i] <- rows$floor_q[i] - 1L
        remain <- remain + 1L
      }
    }
  }

  alloc[rows$bin] <- rows$floor_q
  alloc
}

# ---------------------------
# 1) Load Eurostat population (CZ, 2024, M/F) to 5y bins
# ---------------------------
euro <- fread(eurostat_pop_file, sep = "auto", encoding = "UTF-8", showProgress = FALSE)

# normalize column names (file variants)
setnames(euro, old = intersect(names(euro), c("sex: Sex","sex")), new = "sex", skip_absent = TRUE)
setnames(euro, old = intersect(names(euro), c("geo: Geopolitical entity (reporting)","geo")), new = "geo", skip_absent = TRUE)
setnames(euro, old = intersect(names(euro), c("OBS_VALUE: Observation value","OBS_VALUE")), new = "obs_value", skip_absent = TRUE)
setnames(euro, old = intersect(names(euro), c("TIME_PERIOD: Time","TIME_PERIOD")), new = "time_period", skip_absent = TRUE)
setnames(euro, old = intersect(names(euro), c("age: Age class","age")), new = "age", skip_absent = TRUE)

# keep CZ, 2024, M/F, drop totals/UNK
if (is.character(euro$geo)) {
  # sometimes "CZ: Czechia", keep the code part
  euro[, geo := sub(":.*$", "", geo)]
}
euro <- euro[geo == "CZ"]

# time_period may be year integer or string; normalize to integer year
if (!"time_period" %in% names(euro)) stop("TIME_PERIOD column not found in Eurostat population file.")
euro[, year := suppressWarnings(as.integer(time_period))]
euro <- euro[year == 2024]

# map age to 5y bins
map_pop_age_group <- function(a, val) {
  # a like "Y_LT1","Y_OPEN","Y##","TOTAL","UNK"
  if (a %chin% c("TOTAL","UNK")) return(NA_character_)
  if (a == "Y_LT1") return("0-4")
  if (a == "Y_OPEN") return("90-999")
  if (grepl("^Y[0-9]+$", a)) {
    n <- as.integer(sub("^Y", "", a))
    return(age_group_of_age(n))
  }
  # Some files use plain numbers; try that too
  if (suppressWarnings(!is.na(as.integer(a)))) {
    return(age_group_of_age(as.integer(a)))
  }
  stop(sprintf("Unexpected Eurostat population age label: %s", a))
}

euro[, age_group := vapply(seq_len(.N), function(i) map_pop_age_group(age[i], obs_value[i]), FUN.VALUE = character(1))]
euro <- euro[sex %chin% c("M","F") & !is.na(age_group)]

euro_pop <- euro[, .(eurostat_pop = sum(as.numeric(obs_value), na.rm = TRUE)),
                 by = .(sex, age_group)]

# fill all bins/sex with 0 if missing
euro_pop <- merge(CJ(sex = sexes, age_group = age_levels, unique = TRUE),
                  euro_pop, by = c("sex","age_group"), all.x = TRUE)
euro_pop[is.na(eurostat_pop), eurostat_pop := 0]

euro_total_pop <- euro_pop[, sum(eurostat_pop)]
euro_sex_totals <- euro_pop[, .(e_sex_total = sum(eurostat_pop)), by = sex]
setkey(euro_sex_totals, sex)

# ---------------------------
# 2) Load MZCR population pool (alive on 2024-01-01)
# ---------------------------
orig <- fread(mzcr_origin_file, sep = "auto", encoding = "UTF-8", showProgress = FALSE)
# normalize relevant columns
setnames(orig, old = "ID", new = "id", skip_absent = TRUE)

orig[, `:=`(
  id                = suppressWarnings(as.integer(id)),
  week_date_of_death= as.character(week_date_of_death),
  year_of_birth_end = suppressWarnings(as.integer(year_of_birth_end)),
  gender            = suppressWarnings(as.integer(gender))
)]

# sex from gender (1->M, 2->F, else U)
orig[, sex := fcase(
  gender == 1L, "M",
  gender == 2L, "F",
  default = "U"
)]

# "alive" pool = no death date (blank or NA)
alive <- orig[is.na(week_date_of_death) | week_date_of_death == ""]

# Known YOB (compute age on 2024-01-01)
alive_known <- alive[!is.na(year_of_birth_end)]
alive_known[, age_2024 := 2024 - year_of_birth_end - 1L]
alive_known[age_2024 < 0L, age_2024 := 0L]
alive_known[, age_group := age_group_of_age(age_2024)]

# counts by (sex, age_group), keep U as well (we’ll distribute later)
mzcr_pop_counts <- alive_known[, .N, by = .(sex, age_group)]
setnames(mzcr_pop_counts, "N", "mzcr_pop")

# We only use M/F bins for matching to Eurostat; U counts are *not* in target,
# but we will include them when we add imputed records (after assigning sex).
# Build M/F view with full grid (0 if missing)
mf_pop <- mzcr_pop_counts[sex %chin% sexes]
mf_pop <- merge(CJ(sex = sexes, age_group = age_levels, unique = TRUE),
                mf_pop, by = c("sex","age_group"), all.x = TRUE)
mf_pop[is.na(mzcr_pop), mzcr_pop := 0L]

mzcr_total_pop <- alive_known[, .N]  # known YOB only (current base)
yobs_missing   <- alive[is.na(year_of_birth_end), .N]  # unknown YOB (M/F/U)
# Collect unknown YOB ids in arrival order, partition by current sex flag
alive_unknown <- alive[is.na(year_of_birth_end), .(id, sex)]
ids_M <- alive_unknown[sex == "M", id]
ids_F <- alive_unknown[sex == "F", id]
ids_U <- alive_unknown[sex == "U", id]

# Quick “before” weights (percent of known-YOB pool)
before_weights <- merge(mf_pop, euro_pop, by = c("sex","age_group"), all = TRUE)
before_weights[, `:=`(
  mzcr_weight_pct    = if (mzcr_total_pop > 0) round(mzcr_pop * 100 / mzcr_total_pop, 4) else 0,
  eurostat_weight_pct= if (euro_total_pop  > 0) round(eurostat_pop * 100 / euro_total_pop, 4) else 0
)]

cat("=== BEFORE IMPUTATION: MZCR (known-YOB) vs Eurostat weights ===\n")
print(before_weights)

# ---------------------------
# 3) Impute unknown YOB/sex to match Eurostat weights
# ---------------------------
final_total_pop <- mzcr_total_pop + yobs_missing

# totals by sex (known YOB)
K_sex_total <- mf_pop[, .(k_total = sum(mzcr_pop)), by = sex]
setkey(K_sex_total, sex)

# Eurostat share per sex
e_share <- merge(euro_sex_totals, data.table(sex = sexes), by = "sex", all.y = TRUE)
e_share[is.na(e_sex_total), e_sex_total := 0]
e_share[, sex_share := if (euro_total_pop > 0) e_sex_total / euro_total_pop else 0]

# target final counts by sex
e_share[, target_final_sex := final_total_pop * sex_share]

fix_M <- length(ids_M)
fix_F <- length(ids_F)
need_M <- e_share[sex == "M", max(0, target_final_sex - (K_sex_total["M"]$k_total + fix_M))]
need_F <- e_share[sex == "F", max(0, target_final_sex - (K_sex_total["F"]$k_total + fix_F))]

U_total <- length(ids_U)
cap_M <- ceiling(need_M); cap_F <- ceiling(need_F); cap_sum <- cap_M + cap_F

# Stage S1: fill sex deficits from U
allocU <- c(M = 0L, F = 0L)
if (cap_sum > 0 && U_total > 0) {
  t1 <- min(U_total, cap_sum)
  items <- data.table(
    bin = c("M","F"),
    weight = c(need_M, need_F),
    cap = c(cap_M, cap_F)
  )
  res <- apportion_with_caps(t1, items)
  allocU["M"] <- as.integer(res["M"] %||% 0L)
  allocU["F"] <- as.integer(res["F"] %||% 0L)
}
alloc_fromU <- sum(allocU)

# Stage S2: leftover U by Eurostat sex shares
leftU <- U_total - alloc_fromU
if (leftU > 0) {
  items2 <- data.table(
    bin = c("M","F"),
    weight = c(max(1, e_share[sex == "M", e_sex_total]),
               max(1, e_share[sex == "F", e_sex_total])),
    cap = leftU
  )
  res2 <- apportion_with_caps(leftU, items2)
  allocU["M"] <- allocU["M"] + as.integer(res2["M"] %||% 0L)
  allocU["F"] <- allocU["F"] + as.integer(res2["F"] %||% 0L)
}

# Move U IDs into M/F lists (preserving order), then balance any rounding residue
moveM <- allocU["M"]; moveF <- allocU["F"]
if (moveM > 0 && length(ids_U) > 0) {
  take <- min(moveM, length(ids_U)); ids_M <- c(ids_M, ids_U[seq_len(take)]); ids_U <- ids_U[-seq_len(take)]
}
if (moveF > 0 && length(ids_U) > 0) {
  take <- min(moveF, length(ids_U)); ids_F <- c(ids_F, ids_U[seq_len(take)]); ids_U <- ids_U[-seq_len(take)]
}
while (length(ids_U) > 0) {
  if (length(ids_M) <= length(ids_F)) { ids_M <- c(ids_M, ids_U[1]); ids_U <- ids_U[-1] }
  else                                { ids_F <- c(ids_F, ids_U[1]); ids_U <- ids_U[-1] }
}

# Now distribute per sex across age groups
out_rows <- list()
row_i <- 0L

# Start from current M/F known-YOB counts
mzcr_pop_final <- copy(mf_pop)  # (sex, age_group, mzcr_pop), will be updated

distribute_sex <- function(sex_char, ids_vec) {
  if (length(ids_vec) == 0) return(invisible(NULL))

  # Known counts per age group (this sex)
  K_sg <- mzcr_pop_final[sex == sex_char, .(age_group, mzcr_pop)]
  setkey(K_sg, age_group)
  K_s_total <- sum(K_sg$mzcr_pop, na.rm = TRUE)

  # Eurostat weights for this sex
  W_s <- euro_pop[sex == sex_char, .(age_group, eurostat_pop)]
  setkey(W_s, age_group)
  E_s_total <- sum(W_s$eurostat_pop, na.rm = TRUE)
  if (E_s_total <= 0) {
    # fallback to uniform
    W_s[, eurostat_pop := 1L]
    E_s_total <- nrow(W_s)
  }

  U_s <- length(ids_vec)
  final_s_total <- K_s_total + U_s

  # Stage A: fill deficits towards per-sex Eurostat pattern
  need_items <- data.table(bin = character(), weight = numeric(), cap = integer())
  for (ag in age_levels) {
    k <- K_sg[ag, mzcr_pop][1]
    if (is.na(k)) k <- 0L
    w <- W_s[ag, eurostat_pop][1]
    if (is.na(w)) w <- 0
    target_sg <- final_s_total * (w / E_s_total)
    need <- target_sg - k
    if (need > 0) {
      need_items <- rbind(need_items, data.table(
        bin = ag,
        weight = need,
        cap = ceiling(need)
      ))
    }
  }

  # cap sum must be computed safely (no %||% on vectors)
  t1_cap_sum <- if (nrow(need_items) > 0) sum(need_items$cap, na.rm = TRUE) else 0L
  t1 <- min(U_s, t1_cap_sum)

  # Allocate pass A
  alloc1 <- setNames(integer(length(age_levels)), age_levels)
  if (t1 > 0 && nrow(need_items) > 0) {
    a1 <- apportion_with_caps(t1, need_items)
    alloc1[names(a1)] <- as.integer(a1)
  }

  assigned1 <- sum(alloc1, na.rm = TRUE)
  left <- U_s - assigned1

  # Stage B: leftover by Eurostat weights (fix NA -> 0 before pmax)
  alloc2 <- setNames(integer(length(age_levels)), age_levels)
  if (left > 0) {
    wt_vec <- W_s[.(age_levels), eurostat_pop]
    wt_vec[is.na(wt_vec)] <- 0
    items2 <- data.table(
      bin = age_levels,
      weight = pmax(1, wt_vec),
      cap = left
    )
    a2 <- apportion_with_caps(left, items2)
    alloc2[names(a2)] <- as.integer(a2)
  }

  # Sum per-age allocations
  take_per_ag <- alloc1 + alloc2

  # Assign IDs to bins (ascending by age)
  ids <- ids_vec
  for (ag in age_levels) {
    take <- take_per_ag[ag]
    if (is.na(take) || take <= 0) next
    if (take > length(ids)) take <- length(ids)
    if (take <= 0) next

    ids_take <- ids[seq_len(take)]
    ids <- ids[-seq_len(take)]

    from <- parse_group(ag)$from
    yobe <- 2024 - (from + 1L)

    out_rows[[length(out_rows) + 1L]] <<- data.table(
      id = ids_take,
      sex = sex_char,
      year_of_birth_end = yobe,
      age_group = ag
    )
    mzcr_pop_final[sex == sex_char & age_group == ag, mzcr_pop := mzcr_pop + take]
  }
}

distribute_sex("M", ids_M)
distribute_sex("F", ids_F)

# ---------------------------
# 4) Write imputed population (unknowns only)
# ---------------------------
imputed_dt <- if (length(out_rows)) rbindlist(out_rows, use.names = TRUE) else
  data.table(id=integer(), sex=character(), year_of_birth_end=integer(), age_group=character())

setorder(imputed_dt, id, sex, year_of_birth_end, age_group)
fwrite(imputed_dt, "outputs/imputed_population_yob_2024.csv", quote = FALSE)
cat("Wrote outputs/imputed_population_yob_2024.csv with", nrow(imputed_dt), "imputed records.\n")

# ---------------------------
# 5) Final weights (after adding imputed)
# ---------------------------
final_total_pop <- mzcr_total_pop + yobs_missing

# start from current (post-imputation) counts vs Eurostat
check_dt <- merge(mzcr_pop_final, euro_pop, by = c("sex","age_group"), all = TRUE)
check_dt[is.na(mzcr_pop),     mzcr_pop := 0L]
check_dt[is.na(eurostat_pop), eurostat_pop := 0]

# create weights FIRST (each in its own :=)
check_dt[, mzcr_weight_pct :=
  if (final_total_pop > 0) round(mzcr_pop * 100 / final_total_pop, 4) else 0]

check_dt[, eurostat_weight_pct :=
  if (euro_total_pop  > 0) round(eurostat_pop * 100 / euro_total_pop, 4) else 0]

# then deltas (now the columns exist)
check_dt[, delta_pop := mzcr_pop - eurostat_pop]
check_dt[, delta_weight_pct := round(mzcr_weight_pct - eurostat_weight_pct, 4)]

# per-sex totals (age_group = ALL)
sex_totals <- check_dt[, .(
  mzcr_pop     = sum(mzcr_pop),
  eurostat_pop = sum(eurostat_pop)
), by = .(sex)]

sex_totals[, age_group := "ALL"]
sex_totals[, mzcr_weight_pct     := if (final_total_pop > 0) round(mzcr_pop * 100 / final_total_pop, 4) else 0]
sex_totals[, eurostat_weight_pct := if (euro_total_pop  > 0) round(eurostat_pop * 100 / euro_total_pop, 4) else 0]
sex_totals[, delta_pop           := mzcr_pop - eurostat_pop]
sex_totals[, delta_weight_pct    := round(mzcr_weight_pct - eurostat_weight_pct, 4)]

# overall total (sex = ALL, age_group = ALL)
overall <- data.table(
  sex = "ALL",
  age_group = "ALL",
  mzcr_pop = sum(check_dt$mzcr_pop),
  eurostat_pop = sum(check_dt$eurostat_pop)
)
overall[, mzcr_weight_pct     := if (final_total_pop > 0) round(mzcr_pop * 100 / final_total_pop, 4) else 0]
overall[, eurostat_weight_pct := if (euro_total_pop  > 0) round(eurostat_pop * 100 / euro_total_pop, 4) else 0]
overall[, delta_pop           := mzcr_pop - eurostat_pop]
overall[, delta_weight_pct    := round(mzcr_weight_pct - eurostat_weight_pct, 4)]

# Align column order before rbind
cols <- c("sex","age_group","mzcr_pop","eurostat_pop",
          "mzcr_weight_pct","eurostat_weight_pct","delta_pop","delta_weight_pct")
setcolorder(check_dt,   intersect(cols, names(check_dt)))
setcolorder(sex_totals, intersect(cols, names(sex_totals)))
setcolorder(overall,    intersect(cols, names(overall)))

final_weights <- rbindlist(list(check_dt, sex_totals, overall), use.names = TRUE, fill = TRUE)
fwrite(final_weights, "outputs/final_weights.csv", quote = FALSE)
cat("Wrote outputs/final_weights.csv\n")

# ---------------------------
# Step 5 — Merge all imputations into final dataset
# ---------------------------

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
})

# Inputs
mzcr_origin_file      <- "data/mzcr_no_or_first_infection.csv"
mzcr_deaths_file      <- "outputs/imputation_layer_3.csv"         # known + imputed deaths with YOB
mzcr_imputed_pop_file <- "outputs/imputed_population_yob_2024.csv" # imputed alive (unknown YOB on 2024-01-01)

dir.create("data", showWarnings = FALSE, recursive = TRUE)
out_file <- "data/mzcr_no_or_first_infection_with_imputation.csv"

# --- helpers ---
age_levels <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                "35-39","40-44","45-49","50-54","55-59","60-64",
                "65-69","70-74","75-79","80-84","85-89","90-999")

age_group_of_age <- function(a) {
  a <- as.integer(a)
  as.character(cut(
    a,
    breaks = c(-Inf, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, Inf),
    labels = age_levels,
    right = TRUE
  ))
}

# ---------------------------
# Load imputations
# ---------------------------

# 1) Deaths (layer 3). Should contain one row per id.
d_imp <- fread(mzcr_deaths_file, encoding = "UTF-8")
setnames(d_imp, old = c("id","sex","year_of_birth_end","week_date_of_death","age_at_death",
                        "death_year","death_week","age_group"),
               new = c("id","sex_d","yobe_d","wdd_d","age_at_death_d",
                       "death_year_d","death_week_d","age_group_d"),
         skip_absent = TRUE)
d_imp[, `:=`(
  id     = as.integer(id),
  yobe_d = suppressWarnings(as.integer(yobe_d)),
  wdd_d  = as.character(wdd_d)
)]
# enforce uniqueness
dup_d <- d_imp[duplicated(id), unique(id)]
if (length(dup_d)) stop(sprintf("imputation_layer_3 has duplicate ids: %s", paste(head(dup_d, 10), collapse = ",")))

# 2) Imputed alive population (unknown YOBs only)
p_imp <- fread(mzcr_imputed_pop_file, encoding = "UTF-8")
setnames(p_imp, old = c("id","sex","year_of_birth_end","age_group"),
               new = c("id","sex_p","yobe_p","age_group_p"),
         skip_absent = TRUE)
p_imp[, `:=`(
  id     = as.integer(id),
  yobe_p = suppressWarnings(as.integer(yobe_p))
)]
dup_p <- p_imp[duplicated(id), unique(id)]
if (length(dup_p)) stop(sprintf("imputed_population_yob_2024 has duplicate ids: %s", paste(head(dup_p, 10), collapse = ",")))

# Ensure no overlap
overlap_ids <- intersect(d_imp$id, p_imp$id)
if (length(overlap_ids)) stop(sprintf("IDs present in both death and population imputations: %s", paste(head(overlap_ids, 10), collapse=",")))

# ---------------------------
# Load origin and overlay imputations
# ---------------------------

orig <- fread(mzcr_origin_file, sep = "auto", encoding = "UTF-8", showProgress = FALSE)

# Normalize column names and types expected in output
setnames(orig, old = "ID", new = "id", skip_absent = TRUE)
orig[, `:=`(
  id                 = suppressWarnings(as.integer(id)),
  week_date_of_death = as.character(week_date_of_death),
  year_of_birth_end  = suppressWarnings(as.integer(year_of_birth_end)),
  gender             = suppressWarnings(as.integer(gender)),
  Date_First_Dose                    = as.character(Date_First_Dose),
  Date_Second_Dose                   = as.character(Date_Second_Dose),
  Date_Third_Dose                    = as.character(Date_Third_Dose),
  VaccinationProductCode_First_Dose  = as.character(VaccinationProductCode_First_Dose),
  VaccinationProductCode_Second_Dose = as.character(VaccinationProductCode_Second_Dose),
  VaccinationProductCode_Third_Dose  = as.character(VaccinationProductCode_Third_Dose),
  week_date_of_positivity            = as.character(week_date_of_positivity)
)]

# Derive sex from gender (1->M, 2->F, else U)
orig[, sex := fcase(
  gender == 1L, "M",
  gender == 2L, "F",
  default = "U"
)]

# Overlay deaths imputation first (replaces sex, YOB, week_date_of_death)
setkey(orig, id)
setkey(d_imp, id)
orig <- d_imp[orig]  # left join orig by id with d_imp columns prefixed *_d

# Replace where death imputation exists
orig[!is.na(yobe_d),  year_of_birth_end := yobe_d]
orig[!is.na(sex_d),   sex               := sex_d]
orig[!is.na(wdd_d),   week_date_of_death:= wdd_d]

# Now overlay population imputation for IDs not touched by deaths
setkey(p_imp, id)
orig <- p_imp[orig]   # add *_p columns

orig[ is.na(yobe_d) & !is.na(yobe_p), year_of_birth_end := yobe_p]
orig[ is.na(sex_d)  & !is.na(sex_p),  sex               := sex_p]
# (population imputation has no death date; leave any existing death date as-is)

# ---------------------------
# Compute age / age_at_death / age_group
# ---------------------------

# Age for deaths: death_year - YOB_end, and age_at_death = age
# Age for alive (no death date): 2024 - YOB_end - 1
# Keep week_date_of_death empty if it was empty.

# derive death_date if present (treat "" as NA for calc, but keep "" output)
orig[, death_date_tmp := as.IDate(fifelse(week_date_of_death == "" | is.na(week_date_of_death),
                                          NA_character_, week_date_of_death))]

orig[, `:=`(
  death_year   = ifelse(is.na(death_date_tmp), NA_integer_, isoyear(death_date_tmp)),
  death_week   = ifelse(is.na(death_date_tmp), NA_integer_, isoweek(death_date_tmp))
)]

# age calculations
orig[, age := fifelse(!is.na(death_year),
                      death_year - year_of_birth_end,
                      2024L - year_of_birth_end - 1L)]

# age_at_death only for those with a death date
orig[, age_at_death := fifelse(!is.na(death_year), age, NA_integer_)]

# age_group from age
orig[, age_group := age_group_of_age(age)]

# ---------------------------
# Final checks
# ---------------------------
missing_yob_n <- orig[is.na(year_of_birth_end), .N]
unknown_sex_n <- orig[sex == "U" | is.na(sex), .N]
if (missing_yob_n > 0)
  stop(sprintf("Some rows still have missing year_of_birth_end after imputation: %d", missing_yob_n))
if (unknown_sex_n > 0)
  stop(sprintf("Some rows still have unknown sex ('U') after imputation: %d", unknown_sex_n))

# ---------------------------
# Write final CSV
# ---------------------------
final_cols <- c(
  "id","sex","year_of_birth_end","age","age_group","age_at_death",
  "week_date_of_death",
  "Date_First_Dose","Date_Second_Dose","Date_Third_Dose",
  "VaccinationProductCode_First_Dose","VaccinationProductCode_Second_Dose","VaccinationProductCode_Third_Dose",
  "week_date_of_positivity"
)

# Ensure all columns exist (create empty if origin lacks some)
for (cc in final_cols) if (!cc %in% names(orig)) orig[, (cc) := NA_character_]

setcolorder(orig, final_cols)
fwrite(orig[, ..final_cols], out_file, quote = FALSE, na = "")
cat("Wrote", out_file, "\n")
