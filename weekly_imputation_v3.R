# FITTING THE UNKNOWN COHORT — CLEAN VERSION
# ==================================================
# Deaths: fitted to weekly missing-deaths quotas by age (2020–2023 only)
# Non-deaths: fitted to Jan-2024 Eurostat population age shares
# Canonical age buckets: Unknown, 0–14, 15–24, 25–49, 50–59, 60–69, 70–79, 80+

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(stringr)
  library(tibble)
})

set.seed(42)

cat("=== STEP 1: Load Data ===\n")

wk_missing_age <- read.csv("data/weekly_missing_deaths_among_age_groups.csv", stringsAsFactors = FALSE)
main_data      <- read.csv("data/mzcr_no_or_first_infection.csv", stringsAsFactors = FALSE)

cat(sprintf("Loaded %d records from main cohort\n", nrow(main_data)))

# ---- Robust column access for gender ----
gender_col <- if ("Gender" %in% names(main_data)) "Gender" else if ("gender" %in% names(main_data)) "gender" else NA_character_
if (is.na(gender_col)) stop("No gender column found (expected 'Gender' or 'gender').")

# ---- Dates ----
main_data$week_date_of_death      <- as.Date(main_data$week_date_of_death)
main_data$week_date_of_positivity <- as.Date(main_data$week_date_of_positivity)
main_data$is_death <- !is.na(main_data$week_date_of_death)

# ---- Known/Unknown flags ----
main_data$has_gender <- !is.na(main_data[[gender_col]]) &
  nzchar(trimws(as.character(main_data[[gender_col]]))) &
  !(main_data[[gender_col]] %in% c("", "NA", "Unknown"))

main_data$has_yob <- !is.na(main_data$year_of_birth_end) &
  main_data$year_of_birth_end > 1900 & main_data$year_of_birth_end <= 2024

cat(sprintf("Deaths: %d | Non-deaths: %d\n", sum(main_data$is_death), sum(!main_data$is_death)))

# ==================================================
# STEP 2: Eurostat Jan-2024 population -> living age shares
# ==================================================
eu_file <- "data/demo_pjan_linear_2_0.csv"
eu <- read.csv(eu_file, stringsAsFactors = FALSE, check.names = FALSE)

eu_pop <- eu %>%
  rename(time = `TIME_PERIOD`, value = `OBS_VALUE`) %>%
  mutate(geo_code = sub(":.*$", "", geo),
         sex_raw  = sub(":.*$", "", sex)) %>%
  filter(geo_code == "CZ", time == 2024, age != "TOTAL", age != "UNK") %>%
  mutate(
    age_code = str_extract(age, "Y_LT1|Y_GE\\d+|Y\\d+"),
    age_num = case_when(
      age_code == "Y_LT1" ~ 0L,
      grepl("^Y_GE\\d+$", age_code) ~ as.integer(sub("^Y_GE", "", age_code)),
      grepl("^Y\\d+$", age_code) ~ as.integer(sub("^Y", "", age_code)),
      TRUE ~ NA_integer_
    ),
    value = suppressWarnings(as.numeric(value))
  ) %>%
  filter(!is.na(age_num), !is.na(value)) %>%
  select(age = age_num, sex = sex_raw, pop = value)

# Aggregate to canonical buckets (0–14, 15–24, …)
living_pop_dist <- eu_pop %>%
  mutate(
    age_group = case_when(
      age <= 14 ~ "0–14",
      age <= 24 ~ "15–24",
      age <= 49 ~ "25–49",
      age <= 59 ~ "50–59",
      age <= 69 ~ "60–69",
      age <= 79 ~ "70–79",
      TRUE      ~ "80+"
    )
  ) %>%
  group_by(age_group) %>%
  summarise(pop = sum(pop), .groups = "drop") %>%
  mutate(share = pop / sum(pop)) %>%
  arrange(match(age_group, c("0–14","15–24","25–49","50–59","60–69","70–79","80+")))

cat("\n=== Living Population Age Shares (Eurostat 2024) ===\n")
print(living_pop_dist)

# Optional: sex distribution from Eurostat population (fallback constant if missing)
gender_dist <- tryCatch({
  s <- eu_pop %>% group_by(sex) %>% summarise(n = sum(pop), .groups = "drop")
  tibble(gender = c("F","M"),
         share  = c(ifelse("F" %in% s$sex, s$n[s$sex=="F"], NA),
                    ifelse("M" %in% s$sex, s$n[s$sex=="M"], NA))) %>%
    mutate(share = share / sum(share)) %>%
    tidyr::replace_na(list(share = 0.5))
}, error = function(e) tibble(gender=c("F","M"), share=c(0.5444,0.4556)))

# Constrain death imputations to 2020-W13 .. 2023-12-31
baseline_start <- ISOweek::ISOweek2date("2020-W13-1")
cutoff         <- as.Date("2023-12-31")

# ==================================================
# STEP 3: Death age shares (from weekly missing) & helpers
# ==================================================
# Only use weeks eligible for imputation and known age buckets
age_levels <- c("Unknown","0–14","15–24","25–49","50–59","60–69","70–79","80+")
if (!inherits(wk_missing_age$week_start, "Date"))
  wk_missing_age$week_start <- as.Date(wk_missing_age$week_start)

wk_missing_age <- wk_missing_age %>%
  mutate(age_group = factor(age_group, levels = age_levels)) %>%
  # explicit window (and drop 'Unknown' in target)
  filter(age_group != "Unknown",
         week_start >= baseline_start,
         week_start <= cutoff)

death_age_shares <- wk_missing_age %>%
  group_by(age_group) %>%
  summarise(total_missing = sum(missing_deaths, na.rm = TRUE), .groups = "drop") %>%
  mutate(share = ifelse(sum(total_missing) > 0, total_missing / sum(total_missing), 0))

cat("\n=== Death Age Shares (from weekly quotas, 2020–2023) ===\n")
print(death_age_shares)

# Age ranges (canonical buckets)
age_ranges <- tibble::tibble(
  age_group = c("0–14","15–24","25–49","50–59","60–69","70–79","80+"),
  min_age   = c(0, 15, 25, 50, 60, 70, 80),
  max_age   = c(14, 24, 49, 59, 69, 79, 105)
)

# Util: largest remainder to exact-sum integers
largest_remainder <- function(target, total) {
  target[!is.finite(target)] <- 0
  target <- pmax(target, 0)
  base <- floor(target); rem <- target - base
  diff <- as.integer(total - sum(base))
  base <- as.integer(base)
  if (diff > 0) {
    add_idx <- order(rem, decreasing = TRUE)[seq_len(min(diff, length(rem)))]
    base[add_idx] <- base[add_idx] + 1L
  } else if (diff < 0) {
    rm_idx <- order(rem, decreasing = FALSE)[seq_len(min(-diff, length(rem)))]
    base[rm_idx] <- pmax(base[rm_idx] - 1L, 0L)
  }
  base
}

draw_ages_in_group <- function(n, min_age, max_age) {
  if (n <= 0) integer(0) else sample(min_age:max_age, n, replace = TRUE)
}

# ==================================================
# STEP 4: Prepare imputation columns (idempotent)
# ==================================================
if (!"gender_imputed" %in% names(main_data)) main_data$gender_imputed <- main_data[[gender_col]]
if (!"yob_imputed"   %in% names(main_data)) main_data$yob_imputed     <- main_data$year_of_birth_end
if (!"imputed_flag_gender" %in% names(main_data)) main_data$imputed_flag_gender <- FALSE
if (!"imputed_flag_yob"    %in% names(main_data)) main_data$imputed_flag_yob    <- FALSE

# ==================================================
# STEP 5: EXACT per-week DEATHS allocation (2020–2023 only)
# ==================================================
death_idx       <- which(main_data$is_death)
death_need_yob  <- death_idx[!main_data$has_yob[death_idx]]
cat(sprintf("\nDeaths needing YoB: %d (exact weekly quotas)\n", length(death_need_yob)))

if (length(death_need_yob) > 0) {
  # Candidate pool with their week
  cand <- tibble::tibble(
    rid  = death_need_yob,
    week = main_data$week_date_of_death[death_need_yob]
  ) %>%
    dplyr::filter(!is.na(week),
                  week >= baseline_start,
                  week <= cutoff)


  weeks <- sort(unique(cand$week))
  # Pre-shuffle to avoid order bias
  cand <- cand %>% dplyr::group_by(week) %>%
    dplyr::mutate(ord = sample(dplyr::row_number())) %>%
    dplyr::arrange(week, ord) %>%
    dplyr::ungroup()
  cat(sprintf("  Deaths eligible for YOB imputation in %s..%s: %d\n",
            as.character(baseline_start), as.character(cutoff), nrow(cand)))

  for (w in weeks) {
    pool <- cand %>% dplyr::filter(week == w) %>% dplyr::pull(rid)
    n_unknown <- length(pool)
    if (n_unknown == 0) next

    # Targets for week w (drop Unknown age bucket)
    tgt_week <- wk_missing_age %>%
      dplyr::filter(week_start == w, age_group != "Unknown") %>%
      dplyr::select(age_group, missing_deaths)

    if (nrow(tgt_week) == 0) {
      # Fallback to global death shares if week absent
      tgt_week <- death_age_shares %>%
        dplyr::transmute(age_group, missing_deaths = share * n_unknown)
    } else {
      # Re-scale to match the size of the unknown pool (exact-sum later)
      s <- sum(tgt_week$missing_deaths, na.rm = TRUE)
      if (s <= 0) {
        shares <- death_age_shares$share[match(tgt_week$age_group, death_age_shares$age_group)]
        shares[is.na(shares)] <- 0
        tgt_week$missing_deaths <- shares * n_unknown
      } else {
        tgt_week$missing_deaths <- (tgt_week$missing_deaths / s) * n_unknown
      }
    }

    # Attach ranges and compute integer quotas
    tgt_week <- tgt_week %>% dplyr::left_join(age_ranges, by = "age_group")
    quotas   <- largest_remainder(tgt_week$missing_deaths, n_unknown)

    # Assign exact quotas (SAFE)
    ptr <- 1L
    for (i in seq_len(nrow(tgt_week))) {
      q <- quotas[i]
      if (q <= 0) next

      # Cap to what remains
      remaining <- length(pool) - (ptr - 1L)
      if (remaining <= 0) break
      q_eff <- min(q, remaining)
      if (q_eff <= 0) next

      take_idx <- ptr:(ptr + q_eff - 1L)
      take <- pool[take_idx]
      ptr  <- ptr + q_eff

      # Guard against label mismatches
      if (is.na(tgt_week$min_age[i]) || is.na(tgt_week$max_age[i])) {
        warning(sprintf("Week %s: age_group %s has no range; skipping %d rows.",
                        as.character(w), as.character(tgt_week$age_group[i]), q_eff))
        next
      }

      ages <- draw_ages_in_group(q_eff, tgt_week$min_age[i], tgt_week$max_age[i])
      yobs <- lubridate::year(w) - ages

      take <- take[!is.na(take)]
      if (!length(take)) next

      main_data$yob_imputed[take]      <- as.integer(yobs[seq_along(take)])
      main_data$imputed_flag_yob[take] <- TRUE
    }
  }
}


# Deaths: Gender (probabilistic; make exact with largest_remainder if needed)
death_need_gender <- death_idx[
  !main_data$has_gender[death_idx] &
  !is.na(main_data$week_date_of_death[death_idx]) &
  main_data$week_date_of_death[death_idx] >= baseline_start &
  main_data$week_date_of_death[death_idx] <= cutoff
]
cat(sprintf("Deaths needing Gender (in-window only): %d (probabilistic)\n",
            length(death_need_gender)))
if (length(death_need_gender) > 0) {
  main_data$gender_imputed[death_need_gender]     <- sample(gender_dist$gender, length(death_need_gender), TRUE, gender_dist$share)
  main_data$imputed_flag_gender[death_need_gender] <- TRUE
}

# ==================================================
# STEP 6: NON-DEATHS — exact total fit to Eurostat shares
# ==================================================
nondeath_idx      <- which(!main_data$is_death)
nondeath_need_yob <- nondeath_idx[!main_data$has_yob[nondeath_idx]]
cat(sprintf("\nNon-deaths needing YoB: %d (exact to Eurostat shares)\n", length(nondeath_need_yob)))

if (length(nondeath_need_yob) > 0) {

  # Known non-deaths by age group (on Jan-1 2024, approx: age = 2024 - YOB)
  known_nd <- main_data %>%
    slice(setdiff(nondeath_idx, nondeath_need_yob)) %>%
    filter(!is.na(year_of_birth_end)) %>%
    transmute(age_2024 = 2024 - year_of_birth_end) %>%
    mutate(age_group = cut(age_2024,
                           breaks = c(-Inf, 14, 24, 49, 59, 69, 79, Inf),
                           labels = c("0–14","15–24","25–49","50–59","60–69","70–79","80+"),
                           right = TRUE)) %>%
    count(age_group, name = "known")

  total_nd <- length(nondeath_idx)

  target_totals <- living_pop_dist %>%
    mutate(desired = share * total_nd) %>%
    select(age_group, desired)

  alloc <- target_totals %>%
    left_join(known_nd, by = "age_group") %>%
    mutate(known = tidyr::replace_na(known, 0),
           need_raw = pmax(desired - known, 0))

  quotas <- largest_remainder(alloc$need_raw, length(nondeath_need_yob))

  # Assign exact quotas
  pool <- sample(nondeath_need_yob)  # shuffle
  ptr  <- 1L

  for (i in seq_len(nrow(alloc))) {
    q <- quotas[i]
    if (q <= 0) next

    # Cap quota to remaining pool
    remaining <- length(pool) - (ptr - 1L)
    if (remaining <= 0) break
    q_eff <- min(q, remaining)
    if (q_eff <= 0) next

    take_idx <- ptr:(ptr + q_eff - 1L)
    take <- pool[take_idx]
    ptr  <- ptr + q_eff

    # Map group -> [min,max] with guard
    ix <- match(alloc$age_group[i], age_ranges$age_group)
    if (is.na(ix)) {
      warning(sprintf("Non-death group '%s' not in age_ranges; skipping %d rows.",
                      as.character(alloc$age_group[i]), q_eff))
      next
    }
    rng_min <- age_ranges$min_age[ix]; rng_max <- age_ranges$max_age[ix]

    ages <- draw_ages_in_group(q_eff, rng_min, rng_max)
    yobs <- 2024 - ages

    take <- take[!is.na(take)]
    if (!length(take)) next

    main_data$yob_imputed[take]      <- as.integer(yobs[seq_along(take)])
    main_data$imputed_flag_yob[take] <- TRUE
  }

  # If any leftover due to rounding, drop them in the max-share bin (SAFE)
  if (ptr <= length(pool)) {
    leftover <- pool[ptr:length(pool)]
    if (length(leftover) > 0) {
      top_ix <- which.max(living_pop_dist$share)
      gname  <- living_pop_dist$age_group[top_ix]
      ix <- match(gname, age_ranges$age_group)
      if (!is.na(ix)) {
        ages <- draw_ages_in_group(length(leftover), age_ranges$min_age[ix], age_ranges$max_age[ix])
        main_data$yob_imputed[leftover]      <- as.integer(2024 - ages)
        main_data$imputed_flag_yob[leftover] <- TRUE
      } else {
        warning("Fallback age group not found in age_ranges; leftover non-deaths not assigned.")
      }
    }
  }
}

# Non-deaths: Gender (probabilistic; can be made exact similarly)
nondeath_need_gender <- nondeath_idx[!main_data$has_gender[nondeath_idx]]
cat(sprintf("Non-deaths needing Gender: %d (probabilistic)\n", length(nondeath_need_gender)))
if (length(nondeath_need_gender) > 0) {
  main_data$gender_imputed[nondeath_need_gender]     <- sample(gender_dist$gender, length(nondeath_need_gender), TRUE, gender_dist$share)
  main_data$imputed_flag_gender[nondeath_need_gender] <- TRUE
}

# ==================================================
# STEP 6.5: Final safety fill ONLY for remaining NON-DEATH NA YOBs
# (We do NOT impute YOB for deaths outside 2020-W13..2023-12-31)
# ==================================================
still_na <- which(is.na(main_data$yob_imputed))

if (length(still_na) > 0) {
  # keep ONLY non-deaths
  nondeaths_na <- still_na[ !main_data$is_death[still_na] ]

  # Join ranges to shares to align order
  living_ranges <- age_ranges %>%
    dplyr::left_join(living_pop_dist %>% dplyr::select(age_group, share), by = "age_group") %>%
    dplyr::mutate(share = tidyr::replace_na(share, 0))
  if (sum(living_ranges$share) <= 0) {
    living_ranges$share <- rep(1 / nrow(living_ranges), nrow(living_ranges))
  } else {
    living_ranges$share <- living_ranges$share / sum(living_ranges$share)
  }

  if (length(nondeaths_na) > 0) {
    g_idx <- sample.int(nrow(living_ranges), length(nondeaths_na), replace = TRUE, prob = living_ranges$share)
    ages  <- vapply(g_idx, function(g) sample(living_ranges$min_age[g]:living_ranges$max_age[g], 1L), integer(1))
    yobs  <- 2024 - ages

    main_data$yob_imputed[nondeaths_na]       <- as.integer(yobs)
    main_data$imputed_flag_yob[nondeaths_na]  <- TRUE
  }
}

imputed_deaths_inrange <- with(main_data,
  sum(imputed_flag_yob & is_death &
        !is.na(week_date_of_death) &
        week_date_of_death >= baseline_start &
        week_date_of_death <= cutoff, na.rm = TRUE)
)
imputed_deaths_outside <- with(main_data,
  sum(imputed_flag_yob & is_death &
        !is.na(week_date_of_death) &
        (week_date_of_death < baseline_start | week_date_of_death > cutoff), na.rm = TRUE)
)
cat(sprintf("\nImputed death YOBs in-window:   %d\n", imputed_deaths_inrange))
cat(sprintf("Imputed death YOBs out-of-window (should be 0): %d\n", imputed_deaths_outside))

# ==================================================
# STEP 7: Validation snapshots
# ==================================================
cat("\n=== VALIDATION ===\n")

# BEFORE: valid known YOB only
before_counts <- main_data %>%
  filter(has_yob) %>%                                   # << use valid known YOBs
  transmute(age_2024 = 2024 - year_of_birth_end) %>%
  filter(is.finite(age_2024), age_2024 >= 0, age_2024 <= 120) %>%  # optional safety
  mutate(age_group = cut(age_2024,
                         breaks = c(-Inf, 14, 24, 49, 59, 69, 79, Inf),
                         labels = c("0–14","15–24","25–49","50–59","60–69","70–79","80+"),
                         right = TRUE)) %>%
  count(age_group, name = "before_count")


# AFTER: all with yob_imputed
after_counts <- main_data %>%
  transmute(age_2024 = 2024 - yob_imputed) %>%
  mutate(age_group = cut(age_2024,
                         breaks = c(-Inf, 14, 24, 49, 59, 69, 79, Inf),
                         labels = c("0–14","15–24","25–49","50–59","60–69","70–79","80+"),
                         right = TRUE)) %>%
  count(age_group, name = "after_count")

# IMPUTED ONLY
imputed_only_counts <- main_data %>%
  filter(imputed_flag_yob) %>%
  transmute(age_2024 = 2024 - yob_imputed) %>%
  mutate(age_group = cut(age_2024,
                         breaks = c(-Inf, 14, 24, 49, 59, 69, 79, Inf),
                         labels = c("0–14","15–24","25–49","50–59","60–69","70–79","80+"),
                         right = TRUE)) %>%
  count(age_group, name = "imputed_count")

sanity_check <- before_counts %>%
  full_join(after_counts, by = "age_group") %>%
  full_join(imputed_only_counts, by = "age_group") %>%
  mutate(
    before_count  = replace_na(before_count, 0),
    after_count   = replace_na(after_count, 0),
    imputed_count = replace_na(imputed_count, 0),
    difference    = after_count - before_count,
    check         = difference - imputed_count  # should be 0
  )

cat("\n--- SANITY CHECK: Before/After ---\n")
print(sanity_check)

max_error <- max(abs(sanity_check$check))
pct_error <- 100 * max_error / max(sanity_check$imputed_count, na.rm = TRUE)
if (is.infinite(pct_error)) pct_error <- 0
if (max_error > 100 & pct_error > 0.1) {
  cat(sprintf("\n⚠️  WARNING: Possible re-attribution detected! Max error: %d (%.2f%%)\n", max_error, pct_error))
} else {
  cat(sprintf("\n✓ PASS: No re-attribution detected. Max error: %d (%.3f%%)\n", max_error, pct_error))
}



# Hard guarantees: never impute over VALID known values
stopifnot(sum(main_data$imputed_flag_yob    &  main_data$has_yob) == 0)
stopifnot(sum(main_data$imputed_flag_gender &  main_data$has_gender) == 0)


# No death YOBs imputed outside the allowed window
bad_impute <- with(main_data,
  imputed_flag_yob & is_death &
  !is.na(week_date_of_death) &
  (week_date_of_death < baseline_start | week_date_of_death > cutoff)
)
stopifnot(sum(bad_impute, na.rm = TRUE) == 0)


# Validate DEATHS fit (by age buckets across 2020–2023 unknowns filled)
deaths_val <- main_data %>%
  filter(is_death) %>%
  mutate(
    age_at_death = year(week_date_of_death) - yob_imputed,
    age_group = cut(age_at_death,
                    breaks = c(-Inf, 14, 24, 49, 59, 69, 79, Inf),
                    labels = c("0–14","15–24","25–49","50–59","60–69","70–79","80+"),
                    right = TRUE)
  ) %>%
  count(age_group) %>%
  mutate(fitted_pct = 100 * n / sum(n)) %>%
  left_join(death_age_shares %>% mutate(target_pct = 100 * share), by = "age_group")

cat("\n--- Deaths: fitted vs target shares ---\n")
print(deaths_val, digits = 2)

# Validate NON-DEATHS fit
living_val <- main_data %>%
  filter(!is_death) %>%
  transmute(age_2024 = 2024 - yob_imputed) %>%
  mutate(age_group = cut(age_2024,
                         breaks = c(-Inf, 14, 24, 49, 59, 69, 79, Inf),
                         labels = c("0–14","15–24","25–49","50–59","60–69","70–79","80+"),
                         right = TRUE)) %>%
  count(age_group) %>%
  mutate(fitted_pct = 100 * n / sum(n)) %>%
  left_join(living_pop_dist %>% mutate(target_pct = 100 * share), by = "age_group")

cat("\n--- Living pop: fitted vs target shares ---\n")
print(living_val, digits = 2)

# ==================================================
# STEP 8: Export single final file
# ==================================================
final_data <- main_data %>%
  mutate(
    Gender = case_when(                     # output normalized codes
      gender_imputed %in% c("M","1") ~ "1", # M -> 1
      gender_imputed %in% c("F","2") ~ "2", # F -> 2
      TRUE ~ as.character(gender_imputed)
    ),
    year_of_birth_end = yob_imputed,
    age_at_death = if_else(is_death, year(week_date_of_death) - yob_imputed, NA_integer_)
  ) %>%
  select(-gender_imputed, -yob_imputed, -has_gender, -has_yob)

write.csv(final_data, "data/mzcr_no_or_first_infection_with_imputation.csv", row.names = FALSE, na = "")

cat("\n✓ Output: data/mzcr_no_or_first_infection_with_imputation.csv\n")
cat("\n=== FITTING COMPLETE ===\n")
cat("Deaths: exact to weekly missing-deaths quotas (2020–2023)\n")
cat("Non-deaths: matched to Eurostat Jan-2024 age shares\n")
