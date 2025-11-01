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
use Math::Round qw(nearest);
use FindBin;
use lib "$FindBin::Bin/../lib";

my $no_imput_deaths_file  = "outputs/deaths_non_imputed.csv";
my $imputed_deaths_file   = "outputs/deaths_imputed.csv";
my $offsets_before_file   = 'outputs/non_imputed_deaths_offset.csv';
my $offsets_after_file    = 'outputs/imputed_deaths_offset.csv';
my $imputations_file      = 'outputs/weekly_imputed_deaths.csv';
my $ages_sliding_file     = 'outputs/weekly_ages_sliding.csv';

my @age_groups            = ("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-999");

my %weekly_deaths_offsets = ();
my %mzcr_unknown          = ();
my %aging_slidings        = ();
my %imputations           = ();

my $distrib_extra_unknown = 1;

# --- Offset in deaths when sex & YOB is known ---
load_offsets();

# --- Analyzing sliding & imputation patterns
analyse_sliding_imputations();

# --- Printing report on aging sliding & imputations
print_imputations();
print_aging_slidings();

# --- Imputing & sliding deaths
impute_and_slide_deaths();

sub load_offsets {
    open my $in, '<', $offsets_before_file;
    while (<$in>) {
        chomp $_;
        my ($year, $week, $age_group, $unknown_week_deaths, $mzcr_deaths, $eurostat_deaths, $eurostat_minus_mzcr) = split ',', $_;
        next if $year eq 'year';
        if ($year == 2025) {
            next;
            say "$week, $age_group, $unknown_week_deaths, $mzcr_deaths, $eurostat_deaths, $eurostat_minus_mzcr";
        }
        $mzcr_unknown{$year}->{$week}                                                 = $unknown_week_deaths;
        $weekly_deaths_offsets{$year}->{$week}->{$age_group}->{'mzcr_deaths'}         = $mzcr_deaths;
        $weekly_deaths_offsets{$year}->{$week}->{$age_group}->{'eurostat_deaths'}     = $eurostat_deaths;
        $weekly_deaths_offsets{$year}->{$week}->{$age_group}->{'eurostat_minus_mzcr'} = $eurostat_minus_mzcr;
    }
    close $in;
}

sub analyse_sliding_imputations {
    for my $year (sort{$a <=> $b} keys %weekly_deaths_offsets) {
        for my $week (sort{$a <=> $b} keys %{$weekly_deaths_offsets{$year}}) {

            # say "-" x 50;
            # say "-" x 50;
            # say "-" x 50;
            # say "year                     : $year";
            # say "week                     : $week";
            # say "-" x 50;
            # say "-" x 50;
            # say "-" x 50;

            # --- 1st, sliding excesses
            # If we have too many deaths in a given age group, these must be slided
            # to the nearest age group with sufficient deficit.
            # For example (Week 11, 2021), if in age group 30-34, we have too many deaths
            # in MZCR compared to Eurostat, we'll proceed to the following slidings
            # ---------------------------------------------------------------
            # | Age Group   | MZCR Deaths | Eurostat Deaths |    Sliding    |
            # ---------------------------------------------------------------
            # | 25-29       |  6 deaths   | 7 deaths        |      1        |
            # ---------------------------------------------------------------
            # | 30-34       | 17 deaths   | 10 deaths       |               |
            # ---------------------------------------------------------------
            # | 35-39       | 26 deaths   | 30 deaths       |      4        |
            # ---------------------------------------------------------------
            # --> After reducing the offset by 4+1, the 30-34 MZCR deaths 
            # will therefore be 12 vs 10
            slide_simple_excess($year, $week);
            

            # --- 2nd, above week 11 of 2021, we look for significant positive offsets (>=1 deaths extra in MZCR)
            # impacting a given age group, which should be slided to balance the offsets we mostly
            # observe in the elder age groups, when it's possible ; i.e. when the offset is higher than
            # the total deaths observed in above age groups.
            # For example, week 11 of 2021:
            # ---------------------------------------------------------------------
            # | Age Group   |    MZCR Deaths |    Eurostat Deaths |    Missing    |
            # ---------------------------------------------------------------------
            # | 35-39       |    11 deaths   |     10 deaths      |      -1       |
            # ---------------------------------------------------------------------
            # | 40-44       |    37 deaths   |     37 deaths      |       0       |
            # ---------------------------------------------------------------------
            # | 45-49       |    54 deaths   |     59 deaths      |       5       |
            # ---------------------------------------------------------------------
            # | 50-54       |    80 deaths   |     84 deaths      |       4       |
            # ---------------------------------------------------------------------
            # | 55-59       |   152 deaths   |    152 deaths      |       0       |
            # ---------------------------------------------------------------------
            # | 60-64       |   278 deaths   |    266 deaths      |     -12       |
            # ---------------------------------------------------------------------
            # | 65-69       |   423 deaths   |    423 deaths      |       0       |
            # ---------------------------------------------------------------------
            # | 70-74       |   611 deaths   |    627 deaths      |      16       |
            # ---------------------------------------------------------------------
            # | 75-79       |   623 deaths   |    643 deaths      |      20       |
            # ---------------------------------------------------------------------
            # | 80-84       |   527 deaths   |    572 deaths      |      45       |
            # ---------------------------------------------------------------------
            # | 85-89       |   481 deaths   |    527 deaths      |      46       |
            # ---------------------------------------------------------------------
            # | 90+         |   259 deaths   |    406 deaths      |     147       |
            # ---------------------------------------------------------------------
            # First pass :
            # -----------------------------------------------------------------------------------------------------------------------------------------------------------
            # | Age Group   |    MZCR Deaths |    Eurostat Deaths |    Missing    |    Sliding    | Missing After-Sliding | After-Sliding MZCR | After-Sliding Eurostat |
            # -----------------------------------------------------------------------------------------------------------------------------------------------------------
            # | 35-39       |    11 deaths   |     10 deaths      |      -1       |       1       |           0           |      10 deaths     |       10 deaths        |
            # -----------------------------------------------------------------------------------------------------------------------------------------------------------
            # | 40-44       |    37 deaths   |     37 deaths      |       0       |       1       |           0           |      37 deaths     |       37 deaths        |
            # -----------------------------------------------------------------------------------------------------------------------------------------------------------
            # | 45-49       |    54 deaths   |     59 deaths      |       5       |       1       |           5           |      54 deaths     |       59 deaths        |
            # -----------------------------------------------------------------------------------------------------------------------------------------------------------
            # | 50-54       |    80 deaths   |     84 deaths      |       4       |       1       |           4           |      80 deaths     |       84 deaths        |
            # -----------------------------------------------------------------------------------------------------------------------------------------------------------
            # | 55-59       |   152 deaths   |    152 deaths      |       0       |       1       |           0           |     152 deaths     |      152 deaths        |
            # -----------------------------------------------------------------------------------------------------------------------------------------------------------
            # | 60-64       |   278 deaths   |    266 deaths      |     -12       |       1       |         -12           |     266 deaths     |      266 deaths        |
            # -----------------------------------------------------------------------------------------------------------------------------------------------------------
            # | 65-69       |   423 deaths   |    423 deaths      |       0       |       1       |           0           |     423 deaths     |      423 deaths        |
            # -----------------------------------------------------------------------------------------------------------------------------------------------------------
            # | 70-74       |   611 deaths   |    627 deaths      |      16       |       1       |          16           |     611 deaths     |      627 deaths        |
            # -----------------------------------------------------------------------------------------------------------------------------------------------------------
            # | 75-79       |   623 deaths   |    643 deaths      |      20       |       1       |          20           |     623 deaths     |      643 deaths        |
            # -----------------------------------------------------------------------------------------------------------------------------------------------------------
            # | 80-84       |   527 deaths   |    572 deaths      |      45       |       1       |          45           |     527 deaths     |      572 deaths        |
            # -----------------------------------------------------------------------------------------------------------------------------------------------------------
            # | 85-89       |   481 deaths   |    527 deaths      |      46       |       1       |          46           |     481 deaths     |      527 deaths        |
            # -----------------------------------------------------------------------------------------------------------------------------------------------------------
            # | 90+         |   259 deaths   |    406 deaths      |     147       |       1       |         146           |     260 deaths     |      406 deaths        |
            # -----------------------------------------------------------------------------------------------------------------------------------------------------------
            # Second pass :
            # -----------------------------------------------------------------------------------------------------------------------------------------------------------
            # | Age Group   |    MZCR Deaths |    Eurostat Deaths |    Missing    |    Sliding    | Missing After-Sliding | After-Sliding MZCR | After-Sliding Eurostat |
            # -----------------------------------------------------------------------------------------------------------------------------------------------------------
            # | 35-39       |    10 deaths   |     10 deaths      |       0       |       0       |           0           |      10 deaths     |       10 deaths        |
            # -----------------------------------------------------------------------------------------------------------------------------------------------------------
            # | 40-44       |    37 deaths   |     37 deaths      |       0       |       0       |           0           |      37 deaths     |       37 deaths        |
            # -----------------------------------------------------------------------------------------------------------------------------------------------------------
            # | 45-49       |    54 deaths   |     59 deaths      |       5       |       0       |           5           |      54 deaths     |       59 deaths        |
            # -----------------------------------------------------------------------------------------------------------------------------------------------------------
            # | 50-54       |    80 deaths   |     84 deaths      |       4       |       0       |           4           |      80 deaths     |       84 deaths        |
            # -----------------------------------------------------------------------------------------------------------------------------------------------------------
            # | 55-59       |   152 deaths   |    152 deaths      |       0       |       0       |           0           |     152 deaths     |      152 deaths        |
            # -----------------------------------------------------------------------------------------------------------------------------------------------------------
            # | 60-64       |   278 deaths   |    266 deaths      |     -12       |      12       |           0           |     266 deaths     |      266 deaths        |
            # -----------------------------------------------------------------------------------------------------------------------------------------------------------
            # | 65-69       |   423 deaths   |    423 deaths      |       0       |      12       |           0           |     423 deaths     |      423 deaths        |
            # -----------------------------------------------------------------------------------------------------------------------------------------------------------
            # | 70-74       |   611 deaths   |    627 deaths      |      16       |      12       |          16           |     611 deaths     |      627 deaths        |
            # -----------------------------------------------------------------------------------------------------------------------------------------------------------
            # | 75-79       |   623 deaths   |    643 deaths      |      20       |      12       |          20           |     623 deaths     |      643 deaths        |
            # -----------------------------------------------------------------------------------------------------------------------------------------------------------
            # | 80-84       |   527 deaths   |    572 deaths      |      45       |      12       |          45           |     527 deaths     |      572 deaths        |
            # -----------------------------------------------------------------------------------------------------------------------------------------------------------
            # | 85-89       |   481 deaths   |    527 deaths      |      46       |      12       |          46           |     481 deaths     |      527 deaths        |
            # -----------------------------------------------------------------------------------------------------------------------------------------------------------
            # | 90+         |   260 deaths   |    406 deaths      |     146       |      12       |         134           |     272 deaths     |      406 deaths        |
            # -----------------------------------------------------------------------------------------------------------------------------------------------------------
            if ($year >= 2021 && (($year == 2021 && $week > 10) || $year > 2021)) {
                slide_accross_ages($year, $week);
            }

            # --- 3rd, Imputing unknowns
            # If we have enough deaths in the unknown pool, simply imputing them.
            # Otherwise, determining the % of deaths available, the total of deaths missing in each age group,
            # and imputing the share of the unknown available for each age group.
            # say "imputing unknown : [$year - $week]";
            # Calculating how many deaths are missing.
            my $unknown_week_deaths = $mzcr_unknown{$year}->{$week} // 0;
            # say "unknown_week_deaths      : $unknown_week_deaths";
            impute_unknown($year, $week, $unknown_week_deaths);

            # --- 4th, Distributing left-overs.
            # If there are left-overs, we impute them proportionally to the deaths observed on Eurostat, depending on setting.
            # If we don't have at least 1 death per age group, we simply impute them in reverse order.
            distribute_leftovers($year, $week, $unknown_week_deaths);
        }
    }
}

sub print_imputations {
    open my $out, '>', $imputations_file or die $!;
    say $out "year,week,age_group,imputed";
    for my $year (sort{$a <=> $b} keys %imputations) {
        for my $week (sort{$a <=> $b} keys %{$imputations{$year}}) {
            for my $age_group (sort keys %{$imputations{$year}->{$week}}) {
                my $imputed = $imputations{$year}->{$week}->{$age_group} // die;
                say $out "$year,$week,$age_group,$imputed";
            }
        }
    }
    close $out;
}

sub print_aging_slidings {
    open my $out, '>', $ages_sliding_file or die $!;
    say $out "year,week,origin_age_group,target_age_group,slided";
    for my $year (sort{$a <=> $b} keys %aging_slidings) {
        for my $week (sort{$a <=> $b} keys %{$aging_slidings{$year}}) {
            for my $origin_age_group (sort keys %{$aging_slidings{$year}->{$week}}) {
                for my $target_age_group (sort keys %{$aging_slidings{$year}->{$week}->{$origin_age_group}}) {
                    my $slided = $aging_slidings{$year}->{$week}->{$origin_age_group}->{$target_age_group} // die;
                    say $out "$year,$week,$origin_age_group,$target_age_group,$slided";
                }
            }
        }
    }
    close $out;
}

sub slide_simple_excess {
    my ($year, $week) = @_;
    my $agref = 0;
    for my $age_group (@age_groups) {
        my $mzcr_deaths         = $weekly_deaths_offsets{$year}->{$week}->{$age_group}->{'mzcr_deaths'}         // 0;
        my $eurostat_deaths     = $weekly_deaths_offsets{$year}->{$week}->{$age_group}->{'eurostat_deaths'}     // 0;
        my $eurostat_minus_mzcr = $weekly_deaths_offsets{$year}->{$week}->{$age_group}->{'eurostat_minus_mzcr'} // 0;
        if ($eurostat_minus_mzcr < 0) { # Deaths to slide in this age group, if possible in the next.

            my $to_slide = abs($eurostat_minus_mzcr);
            # say "-" x 50;
            # say "age_group                : $age_group";
            # say "eurostat_deaths          : $eurostat_deaths";
            # say "mzcr_deaths              : $mzcr_deaths";
            # say "to_slide                 : $to_slide";
            # say "-" x 50;

            # First verifying if we can slide to the above age group.
            my $next_ref = $agref + 1;
            my $next_ag  = $age_groups[$next_ref];
            if ($next_ag && $age_group ne '90-999') {
                my $next_mzcr_deaths         = $weekly_deaths_offsets{$year}->{$week}->{$next_ag}->{'mzcr_deaths'}         // 0;
                my $next_eurostat_deaths     = $weekly_deaths_offsets{$year}->{$week}->{$next_ag}->{'eurostat_deaths'}     // 0;
                my $next_eurostat_minus_mzcr = $weekly_deaths_offsets{$year}->{$week}->{$next_ag}->{'eurostat_minus_mzcr'} // 0;
                # say "next_ag                  : $next_ag";
                # say "next_mzcr_deaths         : $next_mzcr_deaths";
                # say "next_eurostat_deaths     : $next_eurostat_deaths";
                # say "next_eurostat_minus_mzcr : $next_eurostat_minus_mzcr";
                if ($next_eurostat_minus_mzcr > 0) {
                    # Full imputation to the next group.
                    if ($next_eurostat_minus_mzcr >= $to_slide) {
                        $next_mzcr_deaths         = $next_mzcr_deaths + $to_slide;
                        $mzcr_deaths              = $mzcr_deaths      - $to_slide;
                        $aging_slidings{$year}->{$week}->{$age_group}->{$next_ag} += $to_slide;
                        $next_eurostat_minus_mzcr = $next_eurostat_deaths - $next_mzcr_deaths;
                        $eurostat_minus_mzcr      = $eurostat_deaths      - $mzcr_deaths;
                        $to_slide                 = 0;
                        # say "---->";
                        # say "next_mzcr_deaths         : $next_mzcr_deaths";
                        # say "next_eurostat_deaths     : $next_eurostat_deaths";
                        # say "next_eurostat_minus_mzcr : $next_eurostat_minus_mzcr";
                        # say "eurostat_deaths          : $eurostat_deaths";
                        # say "mzcr_deaths              : $mzcr_deaths";
                        # say "to_slide                 : $to_slide";
                        $weekly_deaths_offsets{$year}->{$week}->{$next_ag}->{'mzcr_deaths'}           = $next_mzcr_deaths;
                        $weekly_deaths_offsets{$year}->{$week}->{$next_ag}->{'eurostat_minus_mzcr'}   = $next_eurostat_minus_mzcr;
                        $weekly_deaths_offsets{$year}->{$week}->{$age_group}->{'mzcr_deaths'}         = $mzcr_deaths;
                        $weekly_deaths_offsets{$year}->{$week}->{$age_group}->{'eurostat_minus_mzcr'} = $eurostat_minus_mzcr;
                    } else {
                        # Imputes the part available to the next group & subtract to the total to impute.
                        $to_slide                 = $to_slide - $next_eurostat_minus_mzcr;
                        $next_mzcr_deaths         = $next_mzcr_deaths + $next_eurostat_minus_mzcr;
                        $mzcr_deaths              = $mzcr_deaths      - $next_eurostat_minus_mzcr;
                        $aging_slidings{$year}->{$week}->{$age_group}->{$next_ag} += $next_eurostat_minus_mzcr;
                        $next_eurostat_minus_mzcr = $next_eurostat_deaths - $next_mzcr_deaths;
                        $eurostat_minus_mzcr      = $eurostat_deaths      - $mzcr_deaths;
                        $weekly_deaths_offsets{$year}->{$week}->{$next_ag}->{'mzcr_deaths'}           = $next_mzcr_deaths;
                        $weekly_deaths_offsets{$year}->{$week}->{$next_ag}->{'eurostat_minus_mzcr'}   = $next_eurostat_minus_mzcr;
                        $weekly_deaths_offsets{$year}->{$week}->{$age_group}->{'mzcr_deaths'}         = $mzcr_deaths;
                        $weekly_deaths_offsets{$year}->{$week}->{$age_group}->{'eurostat_minus_mzcr'} = $eurostat_minus_mzcr;
                        # say "---->";
                        # say "next_mzcr_deaths         : $next_mzcr_deaths";
                        # say "next_eurostat_deaths     : $next_eurostat_deaths";
                        # say "next_eurostat_minus_mzcr : $next_eurostat_minus_mzcr";
                        # say "eurostat_deaths          : $eurostat_deaths";
                        # say "mzcr_deaths              : $mzcr_deaths";
                        # say "to_slide                 : $to_slide";
                    }
                }
                # say "-" x 50;
            }

            # If we still have deaths to slide, sliding them to the previous age group.
            if ($to_slide && $age_group ne '15-19') {
                my $prev_ref = $agref - 1;
                my $prev_ag  = $age_groups[$prev_ref];
                if ($age_group eq '20-24' && $prev_ag eq '90-999') {
                    die;
                }
                if ($prev_ag) {
                    my $prev_mzcr_deaths         = $weekly_deaths_offsets{$year}->{$week}->{$prev_ag}->{'mzcr_deaths'}         // 0;
                    my $prev_eurostat_deaths     = $weekly_deaths_offsets{$year}->{$week}->{$prev_ag}->{'eurostat_deaths'}     // 0;
                    my $prev_eurostat_minus_mzcr = $weekly_deaths_offsets{$year}->{$week}->{$prev_ag}->{'eurostat_minus_mzcr'} // 0;
                    # say "prev_ag                  : $prev_ag";
                    # say "prev_mzcr_deaths         : $prev_mzcr_deaths";
                    # say "prev_eurostat_deaths     : $prev_eurostat_deaths";
                    # say "prev_eurostat_minus_mzcr : $prev_eurostat_minus_mzcr";
                    if ($prev_eurostat_minus_mzcr > 0) {
                        # Full imputation to the prev group.
                        if ($prev_eurostat_minus_mzcr >= $to_slide) {
                            $prev_mzcr_deaths         = $prev_mzcr_deaths + $to_slide;
                            $mzcr_deaths              = $mzcr_deaths      - $to_slide;
                            if ($age_group eq '20-24' && $prev_ag eq '90-999') {
                                die;
                            }
                            $aging_slidings{$year}->{$week}->{$age_group}->{$prev_ag} += $to_slide;
                            $prev_eurostat_minus_mzcr = $prev_eurostat_deaths - $prev_mzcr_deaths;
                            $eurostat_minus_mzcr      = $eurostat_deaths      - $mzcr_deaths;
                            $to_slide                 = 0;
                            # say "---->";
                            # say "prev_mzcr_deaths         : $prev_mzcr_deaths";
                            # say "prev_eurostat_deaths     : $prev_eurostat_deaths";
                            # say "prev_eurostat_minus_mzcr : $prev_eurostat_minus_mzcr";
                            # say "eurostat_deaths          : $eurostat_deaths";
                            # say "mzcr_deaths              : $mzcr_deaths";
                            # say "to_slide                 : $to_slide";
                            $weekly_deaths_offsets{$year}->{$week}->{$prev_ag}->{'mzcr_deaths'}           = $prev_mzcr_deaths;
                            $weekly_deaths_offsets{$year}->{$week}->{$prev_ag}->{'eurostat_minus_mzcr'}   = $prev_eurostat_minus_mzcr;
                            $weekly_deaths_offsets{$year}->{$week}->{$age_group}->{'mzcr_deaths'}         = $mzcr_deaths;
                            $weekly_deaths_offsets{$year}->{$week}->{$age_group}->{'eurostat_minus_mzcr'} = $eurostat_minus_mzcr;
                        } else {
                            # Imputes the part available to the prev group & subtract to the total to impute.
                            $to_slide                 = $to_slide - $prev_eurostat_minus_mzcr;
                            $prev_mzcr_deaths         = $prev_mzcr_deaths + $prev_eurostat_minus_mzcr;
                            $mzcr_deaths              = $mzcr_deaths      - $prev_eurostat_minus_mzcr;
                            if ($age_group eq '20-24' && $prev_ag eq '90-999') {
                                die;
                            }
                            $aging_slidings{$year}->{$week}->{$age_group}->{$prev_ag} += $prev_eurostat_minus_mzcr;
                            $prev_eurostat_minus_mzcr = $prev_eurostat_deaths - $prev_mzcr_deaths;
                            $eurostat_minus_mzcr      = $eurostat_deaths      - $mzcr_deaths;
                            $weekly_deaths_offsets{$year}->{$week}->{$prev_ag}->{'mzcr_deaths'}           = $prev_mzcr_deaths;
                            $weekly_deaths_offsets{$year}->{$week}->{$prev_ag}->{'eurostat_minus_mzcr'}   = $prev_eurostat_minus_mzcr;
                            $weekly_deaths_offsets{$year}->{$week}->{$age_group}->{'mzcr_deaths'}         = $mzcr_deaths;
                            $weekly_deaths_offsets{$year}->{$week}->{$age_group}->{'eurostat_minus_mzcr'} = $eurostat_minus_mzcr;
                            # say "---->";
                            # say "prev_mzcr_deaths         : $prev_mzcr_deaths";
                            # say "prev_eurostat_deaths     : $prev_eurostat_deaths";
                            # say "prev_eurostat_minus_mzcr : $prev_eurostat_minus_mzcr";
                            # say "eurostat_deaths          : $eurostat_deaths";
                            # say "mzcr_deaths              : $mzcr_deaths";
                            # say "to_slide                 : $to_slide";
                        }
                    }
                    # say "-" x 50;
                }
            }
        }
        $agref++;
    }
}

sub verify_if_group_can_slide {
    my ($year, $week, $origin_age_group, $target_age_group, $to_slide) = @_;
    my $can_slide = 1;
    my $init = 0;
    for my $age_group (@age_groups) {
        if ($age_group eq $target_age_group) {
            last;
        }
        if ($age_group eq $origin_age_group) {
            $init = 1;
            next;
        }
        if ($init == 1) {
            my $mzcr_deaths = $weekly_deaths_offsets{$year}->{$week}->{$age_group}->{'mzcr_deaths'} // 0;
            if ($mzcr_deaths < $to_slide) {
                $can_slide = 0;
                last;
            }
        }
    }
    return $can_slide;
}

sub slide_accross_ages {
    my ($year, $week) = @_;
    # say "sliding                  : [$year - $week]";
    my %processed = ();
    while (1) {

        # Finding youngest age group allowing sliding.
        my ($origin_age_group, $to_slide);
        for my $age_group (@age_groups) {
            next if exists $processed{$age_group}; # If we already processed the age group, skipping it.
            next if $age_group eq '90-999';
            my $mzcr_deaths         = $weekly_deaths_offsets{$year}->{$week}->{$age_group}->{'mzcr_deaths'}         // 0;
            my $eurostat_deaths     = $weekly_deaths_offsets{$year}->{$week}->{$age_group}->{'eurostat_deaths'}     // 0;
            my $eurostat_minus_mzcr = $weekly_deaths_offsets{$year}->{$week}->{$age_group}->{'eurostat_minus_mzcr'} // 0;
            if ($eurostat_minus_mzcr < 0) {        # Deaths to slide in this age group
                $origin_age_group   = $age_group;
                $to_slide           = abs($eurostat_minus_mzcr);
                $processed{$age_group} = 1;
                last;
            }
        }
        last unless $origin_age_group; # Stops processing if we have no group to treat.
        # say "-" x 50;
        # say "-" x 50;
        # say "origin_age_group         : $origin_age_group";
        # say "to_slide                 : $to_slide";
        # say "-" x 50;

        # Determines the targeted age groups (up to where we slide).
        my @target_age_groups = ();
        my $left_to_slide     = $to_slide;
        for my $age_group (reverse @age_groups) {

            # If we have reached the origin age group, we recompute how many people we can slide.
            if ($age_group eq $origin_age_group) {
                # say "Not all can be slided in the above age groups : [$left_to_slide] on [$to_slide] left.";
                $to_slide = $left_to_slide;
                last;
            }
            my $eurostat_minus_mzcr = $weekly_deaths_offsets{$year}->{$week}->{$age_group}->{'eurostat_minus_mzcr'} // 0;
            if ($eurostat_minus_mzcr > 0) {
                my %obj = ();
                $obj{'age_group'} = $age_group;
                if ($eurostat_minus_mzcr < $left_to_slide) {
                    $obj{'to_slide'} = $eurostat_minus_mzcr;
                    $left_to_slide   = $left_to_slide - $eurostat_minus_mzcr;
                    push @target_age_groups, \%obj;
                } else {
                    $obj{'to_slide'} = $left_to_slide;
                    $left_to_slide   = 0;
                    push @target_age_groups, \%obj;
                    last;
                }
            }
        }

        my $total_targets = scalar @target_age_groups;

        # If we don't have any target, skipping this age group.
        unless ($total_targets) {
            next;
        }

        # Verifies if above age groups can be slided (deaths > offset)
        for my $target (reverse @target_age_groups) {
            my $target_age_group = %$target{'age_group'} // die;
            my $to_slide         = %$target{'to_slide'}  // die;
            my $can_slide        = verify_if_group_can_slide($year, $week, $origin_age_group, $target_age_group, $to_slide);
            # say "target_age_group         : $target_age_group";
            # say "to_slide                 : $to_slide";
            # say "-" x 50;

            if ($can_slide) {

                # For each group between origin & target, sliding the scheduled number of subjects.
                my $init  = 0;
                my $agref = 0;
                for my $ag (@age_groups) {
                    next if $ag eq '90-999';
                    if ($ag eq $origin_age_group) {
                        $init = 1;
                    }
                    if ($init == 1) {
                        my $next_ref = $agref + 1;
                        my $next_ag  = $age_groups[$next_ref];
                        $aging_slidings{$year}->{$week}->{$ag}->{$next_ag} += $to_slide;
                    }
                    if ($ag eq $target_age_group) {
                        last;
                    }
                    $agref++;
                }

                # Adjusts the origin & target offsets.
                my $origin_mzcr_deaths         = $weekly_deaths_offsets{$year}->{$week}->{$origin_age_group}->{'mzcr_deaths'}         // 0;
                my $origin_eurostat_deaths     = $weekly_deaths_offsets{$year}->{$week}->{$origin_age_group}->{'eurostat_deaths'}     // 0;
                my $origin_eurostat_minus_mzcr = $weekly_deaths_offsets{$year}->{$week}->{$origin_age_group}->{'eurostat_minus_mzcr'} // 0;
                my $target_mzcr_deaths         = $weekly_deaths_offsets{$year}->{$week}->{$target_age_group}->{'mzcr_deaths'}         // 0;
                my $target_eurostat_deaths     = $weekly_deaths_offsets{$year}->{$week}->{$target_age_group}->{'eurostat_deaths'}     // 0;
                my $target_eurostat_minus_mzcr = $weekly_deaths_offsets{$year}->{$week}->{$target_age_group}->{'eurostat_minus_mzcr'} // 0;
                # say "origin_mzcr_deaths       : $origin_mzcr_deaths";
                # say "origin_eurostat_deaths   : $origin_eurostat_deaths";
                # say "target_mzcr_deaths       : $target_mzcr_deaths";
                # say "target_eurostat_deaths   : $target_eurostat_deaths";
                $target_mzcr_deaths            = $target_mzcr_deaths + $to_slide;
                $origin_mzcr_deaths            = $origin_mzcr_deaths - $to_slide;
                # say "---->";
                # say "origin_mzcr_deaths       : $origin_mzcr_deaths";
                # say "target_mzcr_deaths       : $target_mzcr_deaths";
                $target_eurostat_minus_mzcr    = $target_eurostat_deaths - $target_mzcr_deaths;
                $origin_eurostat_minus_mzcr    = $origin_eurostat_deaths - $origin_mzcr_deaths;
                $weekly_deaths_offsets{$year}->{$week}->{$target_age_group}->{'mzcr_deaths'}         = $target_mzcr_deaths;
                $weekly_deaths_offsets{$year}->{$week}->{$target_age_group}->{'eurostat_minus_mzcr'} = $target_eurostat_minus_mzcr;
                $weekly_deaths_offsets{$year}->{$week}->{$origin_age_group}->{'mzcr_deaths'}         = $origin_mzcr_deaths;
                $weekly_deaths_offsets{$year}->{$week}->{$origin_age_group}->{'eurostat_minus_mzcr'} = $origin_eurostat_minus_mzcr;
            } else { # Simple check that it never happens (we always can slide)
                # say "age_group           : $age_group";
                # say "eurostat_deaths     : $eurostat_deaths";
                # say "mzcr_deaths         : $mzcr_deaths";
                # say "eurostat_minus_mzcr : $eurostat_minus_mzcr";
                p$weekly_deaths_offsets{$year}->{$week};
                die "can't slide"
            }
        }
    }
}

sub impute_unknown {
    my ($year, $week, $unknown_week_deaths) = @_;
    my $missing_accross_ages = 0;
    for my $age_group (reverse @age_groups) {
        my $eurostat_minus_mzcr = $weekly_deaths_offsets{$year}->{$week}->{$age_group}->{'eurostat_minus_mzcr'} // 0;
        if ($eurostat_minus_mzcr > 0) {
            $missing_accross_ages += $eurostat_minus_mzcr;
        }
    }
    if ($missing_accross_ages) {

        # Determining how many subjects we should impute.
        my $to_impute;
        # If we have more people missing than unknowns, we'll limit ourselves to the unknown.
        if ($missing_accross_ages > $unknown_week_deaths) {
            $to_impute = $unknown_week_deaths;
            # say "not enough unknown.";
            # say "missing_accross_ages      : $missing_accross_ages / $unknown_week_deaths available.";
        } else { # Otherwise, we impute the missing people.
            $to_impute = $missing_accross_ages;
            # say "enough unknown.";
            # say "missing_accross_ages      : $missing_accross_ages / $unknown_week_deaths available.";
        }

        # Calculating the share of deaths available for imputation.
        my $available_unknown_percent = $to_impute * 100 / $missing_accross_ages;

        # say "week requiring imputation";
        # say "to_impute                 : $to_impute";
        # say "unknown_week_deaths       : $unknown_week_deaths";
        # say "available_unknown_percent : $available_unknown_percent";

        # Proceeding with imputation.
        my $imputed = 0;
        $weekly_deaths_offsets{$year}->{$week}->{'available_unknown_percent'} = $available_unknown_percent;
        for my $age_group (@age_groups) {
            my $eurostat_minus_mzcr = $weekly_deaths_offsets{$year}->{$week}->{$age_group}->{'eurostat_minus_mzcr'} // 0;
            if ($eurostat_minus_mzcr > 0) {
                my $distributed = nearest(1, $eurostat_minus_mzcr * $available_unknown_percent / 100);
                # say "-" x 50;
                # say "age_group                 : $age_group";

                # Recalculating unknowns & logging imputations.
                $imputed += $distributed;
                # say "eurostat_minus_mzcr       : $eurostat_minus_mzcr";
                # say "distributed               : $distributed";
                if ($imputed > $to_impute) {
                    # Calculating offset.
                    die unless $age_group eq '90-999';
                    my $offset = $imputed - $to_impute;
                    # say "$imputed - $to_impute -> offset : $offset";
                    die "offset : $offset" if $offset > 2;

                    # Reverting imputation.
                    $imputed    -= $distributed;

                    # Recalculating imputation for this age group.
                    $distributed = $distributed - $offset;
                    die if $distributed < 0;
                    $imputed += $distributed;
                    die "$imputed == $to_impute" unless $imputed == $to_impute;
                }

                # Recalculating MZCR deaths & offset after imputation.
                my $mzcr_deaths         = $weekly_deaths_offsets{$year}->{$week}->{$age_group}->{'mzcr_deaths'}         // 0;
                my $eurostat_deaths     = $weekly_deaths_offsets{$year}->{$week}->{$age_group}->{'eurostat_deaths'}     // 0;
                $mzcr_deaths            = $mzcr_deaths + $distributed;
                my $eurostat_minus_mzcr = $eurostat_deaths - $mzcr_deaths;
                $weekly_deaths_offsets{$year}->{$week}->{'age_groups'}->{$age_group}->{'mzcr_deaths'}           = $mzcr_deaths;
                $weekly_deaths_offsets{$year}->{$week}->{'age_groups'}->{$age_group}->{'eurostat_minus_mzcr'}   = $eurostat_minus_mzcr;
                die if $eurostat_minus_mzcr < 0;
                # say "---->";
                # say "mzcr_deaths               : $mzcr_deaths";
                # say "eurostat_deaths           : $eurostat_deaths";
                # say "eurostat_minus_mzcr       : $eurostat_minus_mzcr";
                $imputations{$year}->{$week}->{$age_group} += $distributed;
            }
        }

        # Recalculating unknown after imputation.
        die "$imputed == $to_impute" unless $imputed == $to_impute;
        $unknown_week_deaths = $unknown_week_deaths - $to_impute;
        $mzcr_unknown{$year}->{$week} = $unknown_week_deaths;
    } else {
        # p$weekly_deaths_offsets{$year}->{$week};
        say "no age group requiring imputation on this week";
        say "unknown_week_deaths      : $unknown_week_deaths";
        say "year                     : $year";
        say "week                     : $week";
    }
}

sub distribute_leftovers {
    my ($year, $week, $unknown_week_deaths) = @_;
    my $unknown_week_deaths_left = $mzcr_unknown{$year}->{$week} // 0;
    if ($unknown_week_deaths_left > 0 && $distrib_extra_unknown == 1) {

        # say "week requiring imputation";
        # say "unknown_week_deaths_left : $unknown_week_deaths_left";
        if ($unknown_week_deaths_left < 16) {
            my $imputed = 0;
            for my $age_group (reverse @age_groups) {
                my $eurostat_deaths = $weekly_deaths_offsets{$year}->{$week}->{$age_group}->{'eurostat_deaths'} // 0;
                # say "age_group       : $age_group";
                # say "eurostat_deaths : $eurostat_deaths";
                # say "share           : $share";
                # say "distributed     : $distributed";

                # Recalculating unknowns & logging imputations.
                $imputed += 1;
                # say "eurostat_minus_mzcr       : $eurostat_minus_mzcr";
                # say "imputed                   : $imputed";
                # say "to_impute                 : $to_impute";
                # say "distributed               : $distributed";
                if ($imputed > $unknown_week_deaths_left) {
                    last;
                }

                # Recalculating MZCR deaths & offset after imputation.
                my $mzcr_deaths         = $weekly_deaths_offsets{$year}->{$week}->{$age_group}->{'mzcr_deaths'} // 0;
                # say "mzcr_deaths               : $mzcr_deaths";
                # say "eurostat_deaths           : $eurostat_deaths";
                $mzcr_deaths            = $mzcr_deaths + 1;
                my $eurostat_minus_mzcr = $eurostat_deaths - $mzcr_deaths;
                $weekly_deaths_offsets{$year}->{$week}->{'age_groups'}->{$age_group}->{'mzcr_deaths'}           = $mzcr_deaths;
                $weekly_deaths_offsets{$year}->{$week}->{'age_groups'}->{$age_group}->{'eurostat_minus_mzcr'}   = $eurostat_minus_mzcr;
                $imputations{$year}->{$week}->{$age_group} += 1;
            }
        } else {
            my $total_deaths = 0;
            for my $age_group (@age_groups) {
                my $eurostat_deaths = $weekly_deaths_offsets{$year}->{$week}->{$age_group}->{'eurostat_deaths'} // 0;
                $total_deaths += $eurostat_deaths;
            }
            my $imputed = 0;
            # say "total_deaths             : $total_deaths";
            for my $age_group (@age_groups) {
                my $eurostat_deaths = $weekly_deaths_offsets{$year}->{$week}->{$age_group}->{'eurostat_deaths'} // 0;
                my $share = $eurostat_deaths * 100 / $total_deaths;
                my $distributed = nearest(1, $unknown_week_deaths_left * $share / 100);
                # say "age_group       : $age_group";
                # say "eurostat_deaths : $eurostat_deaths";
                # say "share           : $share";
                # say "distributed     : $distributed";

                # Recalculating unknowns & logging imputations.
                if ($distributed) {
                    $imputed += $distributed;
                    # say "imputed                   : $imputed";
                    # say "to_impute                 : $to_impute";
                    # say "distributed               : $distributed";
                    if ($imputed > $unknown_week_deaths_left) {
                        # Calculating offset.
                        die unless $age_group eq '90-999';
                        my $offset = $imputed - $unknown_week_deaths_left;
                        # say "$imputed - $unknown_week_deaths_left -> offset : $offset";
                        die "offset : $offset" if $offset > 2;

                        # Reverting imputation.
                        $imputed    -= $distributed;

                        # Recalculating imputation for this age group.
                        $distributed = $distributed - $offset;
                        die if $distributed < 0;
                        $imputed += $distributed;
                        die "$imputed == $unknown_week_deaths_left" unless $imputed == $unknown_week_deaths_left;
                    }

                    # If small offset left, attributing it to the eldest.
                    if ($age_group eq "90-999") {
                        if ($imputed < $unknown_week_deaths_left) {
                            my $offset = $unknown_week_deaths_left - $imputed;
                            die "offset : $offset" if $offset > 3;
                            $distributed += $offset;
                            $imputed     += $offset;
                        }
                    }

                    # Recalculating MZCR deaths & offset after imputation.
                    my $mzcr_deaths         = $weekly_deaths_offsets{$year}->{$week}->{$age_group}->{'mzcr_deaths'}         // 0;
                    # say "mzcr_deaths               : $mzcr_deaths";
                    # say "eurostat_deaths           : $eurostat_deaths";
                    $mzcr_deaths            = $mzcr_deaths + $distributed;
                    my $eurostat_minus_mzcr = $eurostat_deaths - $mzcr_deaths;
                    $weekly_deaths_offsets{$year}->{$week}->{'age_groups'}->{$age_group}->{'mzcr_deaths'}           = $mzcr_deaths;
                    $weekly_deaths_offsets{$year}->{$week}->{'age_groups'}->{$age_group}->{'eurostat_minus_mzcr'}   = $eurostat_minus_mzcr;
                    $imputations{$year}->{$week}->{$age_group} += $distributed;
                }
            }

            # Recalculating unknown after imputation.
            die "$imputed == $unknown_week_deaths_left" unless $imputed == $unknown_week_deaths_left;
            $mzcr_unknown{$year}->{$week} = $unknown_week_deaths_left;
        }
    }
}

sub impute_and_slide_deaths {
    my $mzcr_total_rows = 556903;
    my ($cpt, $mzcr_current_rows) = (0, 0);
    open my $in, '<', $no_imput_deaths_file;
    open my $out, '>', $imputed_deaths_file or die $!;
    say $out "id,sex,year_of_birth_end,week_date_of_death,death_year,death_week";
    my ($slided, $imputed) = (0, 0);
    my %stats = ();
    while (<$in>) {
        chomp $_;
        my ($id, $sex, $year_of_birth_end, $week_date_of_death, $death_year, $death_week) = split ',', $_;
        next if $id eq 'id';
        $mzcr_current_rows++;
        $cpt++;
        if ($cpt == 10000) {
            $cpt = 0;
            STDOUT->printflush("\rParsing MZCR Deaths - [$mzcr_current_rows / $mzcr_total_rows]");
        }

        if ($year_of_birth_end) {
            # If age is known, slides it if a sliding is required.
            my $age_at_death          = $death_year - $year_of_birth_end;
            my ($from_year, $to_year) = from_year_to_year_from_age($age_at_death);
            my $age_group             = "$from_year-$to_year";
            if (exists $aging_slidings{$death_year}->{$death_week}->{$age_group}) {
                for my $target_age_group (sort keys %{$aging_slidings{$death_year}->{$death_week}->{$age_group}}) {
                    my $to_slide = $aging_slidings{$death_year}->{$death_week}->{$age_group}->{$target_age_group} // die;
                    $slided++;

                    # Re-attributing year_of_birth_end.
                    my ($from_year_target, $to_year_target) = split '-', $target_age_group;

                    # If the subject must become older, subtracting to his YOB.
                    if ($from_year_target > $from_year) {
                        $year_of_birth_end = $year_of_birth_end - 5;
                    } else {
                        # Otherwise, adding to his YOB.
                        $year_of_birth_end = $year_of_birth_end + 5;
                    }

                    # Re-calculating age & verifying sliding.
                    my $age_recalc = $death_year - $year_of_birth_end;
                    my ($from_year_recalc, $to_year_recalc) = from_year_to_year_from_age($age_recalc);
                    my $age_group_recalc = "$from_year_recalc-$to_year_recalc";
                    $stats{$age_group_recalc}->{'via_sliding'}++;
                    die "$death_year-$death_week: $age_group_recalc ne $target_age_group" unless $age_group_recalc eq $target_age_group;

                    # Subtracting to the slidings left. If the target age group no longer has slidings to perform, clearing key.
                    $to_slide = $to_slide - 1;
                    if ($to_slide == 0) {
                        delete $aging_slidings{$death_year}->{$death_week}->{$age_group}->{$target_age_group};
                    } else {
                        $aging_slidings{$death_year}->{$death_week}->{$age_group}->{$target_age_group} = $to_slide;
                    }

                    # If we no longer have keys in this age group, also clearing age group.
                    my $age_groups_left = keys %{$aging_slidings{$death_year}->{$death_week}->{$age_group}};
                    if ($age_groups_left == 0) {
                        delete $aging_slidings{$death_year}->{$death_week}->{$age_group};
                    }

                    last;
                }
            }
        } else {
            # Impute subject if we have imputations left for this week of death.
            if (exists $imputations{$death_year}->{$death_week}) {
                for my $target_age_group (sort keys %{$imputations{$death_year}->{$death_week}}) {
                    my $to_slide = $imputations{$death_year}->{$death_week}->{$target_age_group} // die;
                    $imputed++;

                    # Re-attributing year_of_birth_end.
                    my ($from_year_target, $to_year_target) = split '-', $target_age_group;

                    # Calculating subject's YOB.
                    $year_of_birth_end = $death_year - $to_year_target;
                    # say "target_age_group  : $target_age_group";
                    # say "from_year_target  : $from_year_target";
                    # say "to_year_target    : $to_year_target";
                    # say "death_year        : $death_year";
                    # say "year_of_birth_end : $year_of_birth_end";

                    # Re-calculating age & verifying sliding.
                    my $age_recalc = $death_year - $year_of_birth_end;
                    my ($from_year_recalc, $to_year_recalc) = from_year_to_year_from_age($age_recalc);
                    my $age_group_recalc = "$from_year_recalc-$to_year_recalc";
                    $stats{$age_group_recalc}->{'via_imput'}++;
                    die "$death_year-$death_week: $age_group_recalc ne $target_age_group" unless $age_group_recalc eq $target_age_group;

                    # Subtracting to the slidings left. If the target age group no longer has slidings to perform, clearing key.
                    $to_slide = $to_slide - 1;
                    if ($to_slide == 0) {
                        delete $imputations{$death_year}->{$death_week}->{$target_age_group};
                    } else {
                        $imputations{$death_year}->{$death_week}->{$target_age_group} = $to_slide;
                    }

                    # If we no longer have keys in this age group, also clearing age group.
                    my $age_groups_left = keys %{$imputations{$death_year}->{$death_week}};
                    if ($age_groups_left == 0) {
                        delete $imputations{$death_year}->{$death_week};
                    }

                    last;

                }
            }
        }

        if ($sex eq 'U') {
            my $rand = nearest(1, rand(1));
            if ($rand == 0) {
                $sex = "M";
            } else {
                $sex = 'F';
            }
        }
        say $out "$id,$sex,$year_of_birth_end,$week_date_of_death,$death_year,$death_week";
    }
    STDOUT->printflush("\rParsing MZCR Deaths - [$mzcr_current_rows / $mzcr_total_rows]");
    say "";
    close $in;
    close $out;

    say "slided                   : $slided";
    say "imputed                  : $imputed";
    p%stats;
}

sub from_year_to_year_from_age {
    my $age = shift;
    my ($from_year, $to_year);
    if ($age <= 4) {
        ($from_year, $to_year) = (0, 4);
    } elsif ($age >= 5 && $age <= 9) {
        ($from_year, $to_year) = (5, 9);
    } elsif ($age >= 10 && $age <= 14) {
        ($from_year, $to_year) = (10, 14);
    } elsif ($age >= 15 && $age <= 19) {
        ($from_year, $to_year) = (15, 19);
    } elsif ($age >= 20 && $age <= 24) {
        ($from_year, $to_year) = (20, 24);
    } elsif ($age >= 25 && $age <= 29) {
        ($from_year, $to_year) = (25, 29);
    } elsif ($age >= 30 && $age <= 34) {
        ($from_year, $to_year) = (30, 34);
    } elsif ($age >= 35 && $age <= 39) {
        ($from_year, $to_year) = (35, 39);
    } elsif ($age >= 40 && $age <= 44) {
        ($from_year, $to_year) = (40, 44);
    } elsif ($age >= 45 && $age <= 49) {
        ($from_year, $to_year) = (45, 49);
    } elsif ($age >= 50 && $age <= 54) {
        ($from_year, $to_year) = (50, 54);
    } elsif ($age >= 55 && $age <= 59) {
        ($from_year, $to_year) = (55, 59);
    } elsif ($age >= 60 && $age <= 64) {
        ($from_year, $to_year) = (60, 64);
    } elsif ($age >= 65 && $age <= 69) {
        ($from_year, $to_year) = (65, 69);
    } elsif ($age >= 70 && $age <= 74) {
        ($from_year, $to_year) = (70, 74);
    } elsif ($age >= 75 && $age <= 79) {
        ($from_year, $to_year) = (75, 79);
    } elsif ($age >= 80 && $age <= 84) {
        ($from_year, $to_year) = (80, 84);
    } elsif ($age >= 85 && $age <= 89) {
        ($from_year, $to_year) = (85, 89);
    } else {
        die unless $age >= 90;
        ($from_year, $to_year) = (90, 999);
    }
    return ($from_year, $to_year);
}