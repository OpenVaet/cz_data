#!/usr/bin/perl
use strict;
use warnings;
use 5.30.0;
no autovivification;
binmode STDOUT, ":utf8";
use utf8;
use Text::CSV_XS;
use Data::Printer;
use Scalar::Util qw(looks_like_number);
use File::Spec;
use Time::Piece;
use Time::Seconds qw(ONE_DAY);
use FindBin;
use lib "$FindBin::Bin/../lib";

# Step 1 :
# 1. We read deaths from eurostat & classify them by sex & age.
# 2. We read deaths from MZCR, and only care about those for which the year of birth is known
# 3. We compare each week's total, and compute the weeks where we already have more deaths on MZCR than on Eurostat
system("perl imputation_1.pl");

# Step 2 :
# 1. We load the offsets we previously computed.
# 2. We re-attribute offsets which can be compensated - by balancing the deaths (8,909) when too many people died in a MZCR age group, compared to Eurostat, to the MZCR age group immediately before or after.
system("perl imputation_2.pl");

# Step 3 :
# 1. We load the eurostat deaths and classify them by year, week, age group & sex.
# 2. We load our previously reimputed deaths by age groups & sexes ; and evaluate on which days we have too many - or not enough known YOB deaths after reattribution.
# 3. We load the overall data, and note total deaths by age groups & sexes, and those for which we don't have the YOB, along with IDs on which we have no YOB
# 4. For each week which doesn't have enough deaths in a given age group & sex, we attribute from the unknown cohort who died this week the available share of deaths,
#    proportional to the distribution reported by Eurostat on this specific week.
# 5. We distribute the left-overs of the deaths observed on a given week, proportionally to the share of deaths by age groups reported by Eurostat for this week.
system("perl imputation_3.pl");

# Step 4 :
# 1. We distribute the unknown population alive on January 1st 2024, so that the weight of each 5-year age group fits in MZCR & Eurostat.
system("perl imputation_4.pl");

# Step 5 :
# 1. We merge the data resulting from the 3 layers of outputs (deaths, population, non-imputed subjects)
system("perl imputation_5.pl");