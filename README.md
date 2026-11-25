# Step 24 · Combined missing-age and death-linkage assumptions during Delta

Run `Rscript 24_delta_combined_sensitivity.R` from the unpacked bundle, or source that file in RStudio. The script uses base R and the two supplied CSVs in `step24_inputs/`. It writes the results to `results/`. Set `CZ_COMBINED_DELTA_OUT` to change the output location.

## Calculation

The cohort is November NZIP. The death week comes from the LPZ death-certificate field. A record with an ISIN surveillance-death week is flagged. The comparison is no recorded first dose at the LPZ week divided by at least one recorded dose, with Step 21/22 eligibility and same-week rules.

1. For each of the 200 weeks (2020-W10 to 2023-W52) and each recorded dose status, allocate deaths without a birth band in proportion to all known-age LPZ deaths, and allocate unknown-band person-days in proportion to known-age person-days. The allocations include children before the analysis selects ages 15+. Deaths default to person-day weights in a week/status with no known-age deaths. All eligible missing-band LPZ deaths were unflagged and had no recorded dose. Step 05 confirmed that the one vaccinated record missing a band had no death date.
2. Repeat with the age bands left missing, with double allocation weights at ages 65+, and with half of the entirely undated no-dose person-time included. The half-time variant uses proportional age allocation. Each model uses the no-dose and vaccinated exposure implied by its age rule.
3. On each age-allocation result, refit the Step 22 odds shift by endpoint and five-year age band through 90+. Calibrate over all 200 weeks. A 15% target means transferred deaths divided by the final vaccinated death count. In the equal-share case each endpoint has that target. The differential cases target 7.5% or zero among flagged deaths and allocate the balance to unflagged deaths to reach 15% overall in each band. A flagged target is undefined in a band with no observed vaccinated flagged death and no transfer. Such bands have a blank flagged share in the audit. Person-time is fixed during this second step. Each age-week-endpoint cell keeps its total number of deaths.
4. Within each age band and recorded dose status, pool weeks 47–50 of 2021 before directly standardizing to ESP 2013, normalized for ages 15+. Output rates are per 100,000 person-years.

These are sensitivity assumptions about age, time under observation and death linkage. Ratios describe the recorded groups. The range across assumptions is not a confidence interval.

## Reproduction

- `results/delta_period_rates.csv` gives the four ASMRs, the two no-dose to vaccinated ratios and their ratio for all 16 combinations.
- `results/full_study_calibration.csv` gives transferred deaths and achieved shares by age and scenario.
- `results/delta_cells.csv` gives the period’s underlying weekly deaths and person-days.
- `delta_combined_section_edits.html` gives the suggested insert for Section 7 and replacement sentence for Section 8.

To regenerate the supplied input summaries, run `Rscript prepare_step24_inputs.R "Step05_output_directory" "Step07_output_directory" "Step22_output_directory"`. The helper verifies missing-band counts against the Step 05 field tabulations and reconciles the adult cells to Step 22. It writes only aggregate data. Known-age denominators and total LPZ deaths for all ages come from Step 07. Adult flagged deaths come from Step 22. The three child age bands carry no flagged split in the bundled file, because only their total deaths and total person-time are needed to allocate unknown age.

The verification run rebuilt the Step 22 birth-cohort aggregate from the public November release, matched its published script checksum (537483dae653f53d2db07db56d84f04233d04ac0306454a1a8ecd0e4381f8e01), and reproduced Steps 22 and 23 when each adjustment was run alone. An independent calculation checked all 32 endpoint rate ratios, including recalibration using a solver on the odds-multiplier scale.

[November release and dictionary](https://www.nzip.cz/data/2135-covid-19-prehled-populace) · [Article project](https://github.com/OpenVaet/cz_data)
