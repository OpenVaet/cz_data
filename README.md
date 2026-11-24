# Czech all-cause mortality by recorded COVID-19 vaccination status

This R project compares two Czech administrative releases with national counts of deaths, vaccinations and residents. It asks how closely the sources agree and how estimates of mortality by recorded dose status change when age, population coverage or linkage assumptions change. The March release supports mortality comparisons through 2022. The November release extends the analysis through 2023.

## Reproduce the figures and tables

Open `cz_data.Rproj` in RStudio, or start R in the repository root. The checked-in snapshot contains the aggregate inputs used for the article. Once the R packages are installed, this build runs offline.

```r
install.packages(c("png", "digest"))  # first use
Sys.setenv(CZ_PUBLICATION_INPUTS = "snapshot")
export_dir <- source("16_substack_assets.R", encoding = "UTF-8")$value
file.path(export_dir, "substack_insertion_guide.html")
```

Open the HTML file at the printed path. It contains figure previews, captions and selectable table values. The adjacent `png/` directory holds the images, and `outputs.csv` records their SHA-256 checksums. The article uses **V01–V19** and **T01–T09**. The export also retains supplements and the historical V20 figure for reference.

| To inspect | Open |
| --- | --- |
| Census, ageing and migration | V01–V02 and T01 |
| Agreement on doses, deaths and infections, and November population coverage | V03–V09 |
| Mortality in March and November during 2020–2022 | V10 and T02 |
| Annual deaths, person-time and age-standardized mortality rates (ASMR) | V11, T03 and T06–T09 |
| Certified COVID-19 deaths and 2023 mortality by recorded dose status | T04 and T05 |
| Original allocation and the 15% missed-linkage scenario | V12–V18 by age group and V19 for the weekly ASMR at ages 15+ |

## Repeat the three checks behind the discussion

Run this in the same R session as the snapshot build. Step 16 already calls Step 17. Calling Step 17 again verifies its saved output and returns the directory that Step 20 needs.

```r
source("18_verify_younger_no_dose_excess.R", encoding = "UTF-8")
source("19_check_migration_and_mortality.R", encoding = "UTF-8")
asmr_dir <- source("17_november_linkage_adjusted_asmr.R", encoding = "UTF-8")$value
Sys.setenv(CZ_STEP17_DIR = asmr_dir)
source("20_check_age_group_rate_ratios.R", encoding = "UTF-8")
```

Each script prints its output path under `data/analysis/`. Step 18 separates the deaths and person-time added when moving from known birth bands to the resident estimate at ages 25–49. Step 19 compares mortality shortfalls with registered migration by age. Step 20 checks how the 2023 rate ratios change when both dose groups receive the same age weights.

## Rebuild from the source records

Use a fresh R session in the repository root. This route downloads the source files and requires several GB of free disk space. The November CSV alone is about 1.4 GB. To reuse publisher-named downloads, set `CZ_INPUT_DIR` to their directory before running Step 01.

```r
install.packages(c("data.table", "digest", "jsonlite", "openxlsx", "png"))
Sys.setenv(CZ_SOURCE_SET = "all", CZ_PUBLICATION_INPUTS = "project",
           CZ_MISSED_DEATH_FRACTION = "0.15", CZ_LINKAGE_CALIBRATION = "study")

for (step in c(1:10, 12:16)) {
  script <- list.files(pattern = sprintf("^%02d_.*[.]R$", step))
  stopifnot(length(script) == 1L)
  source(script, encoding = "UTF-8")
}
```

Steps 01–02 acquire and prepare the releases. Steps 03–06 check cohorts, event counts, missing demographics and resident coverage. Steps 07–09 build the age-specific rates and compare releases and years. Step 10 reproduces a legacy calculation retained in the export. Steps 12–15 construct the resident-based estimates, the 2015–2019 seasonal comparison and the linkage scenario. Step 16 renders the publication assets and invokes Step 17. The numbering skips 11.

Step 01 writes dated manifests under `data/sources/manifests/` with source URLs, acquisition status and hashes. A failed download can be retried by rerunning Step 01. Successful downloads are cached. Step 02 writes to `data/prepared/`. Later runs write to `data/analysis/`. National series can be revised, so retain the source manifest when reporting a fresh result. The [acquisition script](01_acquire_sources.R) lists the sources and accepted filenames. The [verification directory](verification/) contains the reference manifests and release checks.

## Data and interpretation

**March ÚZIS freedom-of-information extract.** Records have a single birth year. ÚZIS reconstructed part of the population from demographic totals. Death dates end in December 2022, although vaccination dates extend into March 2024. [ÚZIS response](https://www.uzis.cz/res/file/poskytnute-informace/24-06-inf106-1999-odpoved.pdf)

**November NZIP release.** Events are reported by ISO week and birth years in five-year bands. Some records lack a birth band. Repeat infection episodes can produce more than one row per person. Step 02 separates infection episodes from the records used to count doses and deaths. [November field dictionary](https://www.nzip.cz/data/isin-covid/metodicke-popisy/Otevrena-data-NR-26-30-covid-19-prehled-populace-2024-01.pdf)

**National comparisons.** Eurostat supplies weekly resident deaths and January population estimates. The Czech Statistical Office supplies census, migration and certified cause-of-death figures. Ministry series provide published totals against which to check the doses and infections counted in the releases. The dose comparison also examines ECDC reporting by age. [Eurostat deaths](https://ec.europa.eu/eurostat/databrowser/view/demo_r_mwk_05/default/table?lang=en) · [Eurostat population](https://ec.europa.eu/eurostat/databrowser/view/demo_pjan/default/table?lang=en) · [Ministry open data](https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19) · [Czech cause-of-death sources](publication_source/csu_g02_sources.csv)

**Mortality estimates.** The direct calculation uses deaths and person-time within each release. On the Eurostat resident basis, estimated no-dose deaths and person-time are the respective resident totals minus the amounts assigned after a recorded first dose. Ages refer to attained age. The primary estimates distribute people across the ages compatible with their birth information and track ageing over time. Rates are per 100,000 person-years. The ASMR gives both groups the same 2013 European Standard Population weights for the included ages.

**The 15% scenario.** A 15.57% shortfall against seasonal expectation among people aged 25–49 after a recorded first dose in 2023 motivated the round figure. Step 15 asks what would happen if 15% of all modelled deaths after a first dose had missed that link over the full study within each five-year age band, with ages 90+ pooled. Weekly transfers preserve resident deaths and hold person-time fixed. The records do not establish the actual error frequency. Health differences between dose groups, migration and incomplete coverage also limit causal interpretation. The 95% intervals in T03 and T05 describe counting variation conditional on the exposure and age allocations.
