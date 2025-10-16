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
use File::Path qw(make_path);
use File::Spec;
use Time::Piece;
use Math::Round qw(nearest);
use Time::Seconds qw(ONE_DAY);
use FindBin;
use lib "$FindBin::Bin/../lib";

my $eurostat_deaths_file = "data/demo_r_mwk_05_linear_2_0.csv";
my $mzcr_file            = 'outputs/imputation_layer_2.csv';
my $mzcr_origin_file     = "data/mzcr_no_or_first_infection.csv";

my %eurostat_deaths = ();   # {year}{week}{age_group}{sex} => eurostat deaths
my %mzcr_deaths     = ();
my %data_to_attribute = ();

my $csv;
BEGIN {
    eval { require Text::CSV_XS; Text::CSV_XS->import(); 1 }
      ? ($csv = Text::CSV_XS->new({ binary => 1, eol => "\n" }))
      : do { require Text::CSV; Text::CSV->import(); $csv = Text::CSV->new({ binary => 1, eol => "\n" }) };
}

load_eurostat_deaths();

my %known_yob_data = ();
load_mzcr_known_yob_deaths();

my ($total_deaths_extra, $total_deaths_missing) = (0, 0);
my %weeks_requiring_imputation = ();

# --- Age groups & sexes ---
my @age_groups = ("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-999");
my @sexes = ('M','F');

compute_age_groups_sexes_offsets();

# --- Recap ---
$total_deaths_extra   = abs($total_deaths_extra);
$total_deaths_missing = abs($total_deaths_missing);
say "-" x 50;
say "----- Post-imputation 2 Recap. -----";
say "-" x 50;
say "total_deaths_extra       : $total_deaths_extra";
say "total_deaths_missing     : $total_deaths_missing";
say "-" x 50;

# --- MZCR Original file ---
my %no_yob_deaths = ();  # {year}{week}{sex}{...}
my %no_yob_ids    = ();  # {id}{...}
load_no_yob_deaths();

# --- Weekly deficit analysis (stage A/B planning) ---
my %weekly_deaths_imputations = ();
calculate_weekly_imputations();

# --- Output planning details (optional) ---
print_imputation_details();

# --- Output already-known (with YOB) + impute unknowns ---
open my $out_imput, '>', 'outputs/imputation_layer_3.csv';
say $out_imput "id,sex,year_of_birth_end,week_date_of_death,age_at_death,death_year,death_week,age_group";
print_known_dob_deaths();

# --- Stage A/B per-ID imputation + collect leftovers for Stage C ---
impute_to_unknown_yob_sex();

close $out_imput;
say "";

# =========================
# ======== helpers ========
# =========================
sub ymd_to_iso_year_week {
    my ($ymd) = @_;
    die "Bad date: $ymd" unless defined $ymd && $ymd =~ /\A\d{4}-\d{2}-\d{2}\z/;
    my $t = Time::Piece->strptime($ymd, "%Y-%m-%d");
    my $iso_week = 0 + $t->strftime("%V");
    my $iso_wday = 0 + $t->strftime("%u");
    my $thu      = $t + (4 - $iso_wday) * ONE_DAY;
    my $iso_year = 0 + $thu->strftime("%Y");
    return ($iso_year, $iso_week);
}

sub open_csv_reader {
    my ($path) = @_;
    open my $fh, "<:encoding(UTF-8)", $path or die "Can't open $path: $!";
    my $header_line = <$fh>;
    die "Empty CSV: $path" unless defined $header_line;
    $header_line =~ s/^\x{FEFF}//;
    $header_line =~ s/^\xEF\xBB\xBF//;
    my $sep_char = ',';
    if ($header_line =~ /;/ && $header_line !~ /,/) { $sep_char = ';'; }
    elsif ($header_line =~ /\t/ && $header_line !~ /,|;/) { $sep_char = "\t"; }

    my $csv = Text::CSV_XS->new({
        binary => 1, auto_diag => 1, sep_char => $sep_char,
        quote_char => '"', escape_char => '"',
        allow_loose_quotes => 1, allow_loose_escapes => 1, blank_is_undef => 0,
    }) or die "Text::CSV_XS->new failed: " . Text::CSV_XS->error_diag;

    $csv->parse($header_line) or die "Header parse failed for $path: " . $csv->error_diag;
    my @headers = map { my $h = $_ // ''; $h =~ s/^\s+|\s+$//g; $h; } $csv->fields;
    $csv->column_names(\@headers);

    my %col_idx; for (my $i = 0; $i < @headers; $i++) { $col_idx{$headers[$i]} = $i; }
    return ($csv, $fh, \@headers, \%col_idx);
}

sub from_year_to_year_from_age {
    my $age = shift;
    my ($f,$t);
    if    ($age <= 4)               { ($f,$t)=(0,4)   }
    elsif ($age<=9)                 { ($f,$t)=(5,9)   }
    elsif ($age<=14)                { ($f,$t)=(10,14) }
    elsif ($age<=19)                { ($f,$t)=(15,19) }
    elsif ($age<=24)                { ($f,$t)=(20,24) }
    elsif ($age<=29)                { ($f,$t)=(25,29) }
    elsif ($age<=34)                { ($f,$t)=(30,34) }
    elsif ($age<=39)                { ($f,$t)=(35,39) }
    elsif ($age<=44)                { ($f,$t)=(40,44) }
    elsif ($age<=49)                { ($f,$t)=(45,49) }
    elsif ($age<=54)                { ($f,$t)=(50,54) }
    elsif ($age<=59)                { ($f,$t)=(55,59) }
    elsif ($age<=64)                { ($f,$t)=(60,64) }
    elsif ($age<=69)                { ($f,$t)=(65,69) }
    elsif ($age<=74)                { ($f,$t)=(70,74) }
    elsif ($age<=79)                { ($f,$t)=(75,79) }
    elsif ($age<=84)                { ($f,$t)=(80,84) }
    elsif ($age<=89)                { ($f,$t)=(85,89) }
    else                            { die unless $age>=90; ($f,$t)=(90,999) }
    return ($f,$t);
}

sub load_eurostat_deaths {
    my ($euro_csv, $euro_fh, $euro_headers, $euro_idx) = open_csv_reader($eurostat_deaths_file);
    while (my $row = $euro_csv->getline_hr($euro_fh)) {
        my $sex = $row->{'sex: Sex'} // die;
        my $geo = $row->{'geo: Geopolitical entity (reporting)'} // die;
        my $value = $row->{'OBS_VALUE: Observation value'} // die;
        my $week_code = $row->{'TIME_PERIOD: Time'} // die;
        my $age = $row->{'age: Age class'} // die;
        next unless $geo eq 'CZ: Czechia';
        next if $sex eq 'T: Total';
        next if $age eq 'TOTAL: Total';
        my $sex_code = $sex eq 'F: Females' ? 'F' : $sex eq 'M: Males' ? 'M' : die;

        my ($year, $week) = split '-', $week_code;
        my ($f,$t);
        if ($age eq "UNK: Unknown") { die if $value; next; }
        elsif ($age eq "Y_LT5: Less than 5 years")               { ($f,$t)=(0,4) }
        elsif ($age eq "Y5-9: From 5 to 9 years")                 { ($f,$t)=(5,9) }
        elsif ($age eq "Y10-14: From 10 to 14 years")             { ($f,$t)=(10,14) }
        elsif ($age eq "Y15-19: From 15 to 19 years")             { ($f,$t)=(15,19) }
        elsif ($age eq "Y20-24: From 20 to 24 years")             { ($f,$t)=(20,24) }
        elsif ($age eq "Y25-29: From 25 to 29 years")             { ($f,$t)=(25,29) }
        elsif ($age eq "Y30-34: From 30 to 34 years")             { ($f,$t)=(30,34) }
        elsif ($age eq "Y35-39: From 35 to 39 years")             { ($f,$t)=(35,39) }
        elsif ($age eq "Y40-44: From 40 to 44 years")             { ($f,$t)=(40,44) }
        elsif ($age eq "Y45-49: From 45 to 49 years")             { ($f,$t)=(45,49) }
        elsif ($age eq "Y50-54: From 50 to 54 years")             { ($f,$t)=(50,54) }
        elsif ($age eq "Y55-59: From 55 to 59 years")             { ($f,$t)=(55,59) }
        elsif ($age eq "Y60-64: From 60 to 64 years")             { ($f,$t)=(60,64) }
        elsif ($age eq "Y65-69: From 65 to 69 years")             { ($f,$t)=(65,69) }
        elsif ($age eq "Y70-74: From 70 to 74 years")             { ($f,$t)=(70,74) }
        elsif ($age eq "Y75-79: From 75 to 79 years")             { ($f,$t)=(75,79) }
        elsif ($age eq "Y80-84: From 80 to 84 years")             { ($f,$t)=(80,84) }
        elsif ($age eq "Y85-89: From 85 to 89 years")             { ($f,$t)=(85,89) }
        elsif ($age eq "Y_GE90: 90 years or over")                { ($f,$t)=(90,999) }
        else { die }
        my $age_group = "$f-$t";
        $week =~ s/W//;
        die unless looks_like_number($week);
        $year = 0 + $year; $week = 0 + $week;
        $eurostat_deaths{$year}->{$week}->{$age_group}->{$sex_code} += $value;
    }
    $euro_csv->eof or die "CSV error in $eurostat_deaths_file: " . $euro_csv->error_diag;
    close $euro_fh or warn "Couldn't close $eurostat_deaths_file: $!";
}

sub load_mzcr_known_yob_deaths {
    my $mzcr_total_rows = 493208;
    my ($cpt, $mzcr_current_rows) = (0, 0);
    my ($mzcr_csv, $mzcr_fh, $mzcr_headers, $mzcr_idx) = open_csv_reader($mzcr_file);
    while (my $row = $mzcr_csv->getline_hr($mzcr_fh)) {
        $mzcr_current_rows++; $cpt++;
        if ($cpt == 1000) { $cpt = 0; STDOUT->printflush("\rParsing MZCR - [$mzcr_current_rows / $mzcr_total_rows]"); }
        my $week_date_of_death = $row->{'week_date_of_death'} // die;
        my $year_of_birth_end  = $row->{'year_of_birth_end'}  // die;
        my $age_at_death       = $row->{'age_at_death'}       // die;
        my $sex                = $row->{'sex'}                // die;
        my $id                 = $row->{'id'}                 // die;
        my ($death_year, $death_week) = ymd_to_iso_year_week($week_date_of_death);
        my ($f,$t) = from_year_to_year_from_age($age_at_death);
        my $age_group = "$f-$t";
        $mzcr_deaths{$death_year}->{$death_week}->{$age_group}->{$sex}++;
        $known_yob_data{$id} = {
            week_date_of_death => $week_date_of_death,
            year_of_birth_end  => $year_of_birth_end,
            age_at_death       => $age_at_death,
            sex                => $sex,
            death_year         => $death_year,
            death_week         => $death_week,
            age_group          => $age_group
        };
    }
    $mzcr_csv->eof or die "CSV error in $mzcr_file: " . $mzcr_csv->error_diag;
    close $mzcr_fh or warn "Couldn't close $mzcr_file: $!";
    STDOUT->printflush("\rParsing MZCR - [$mzcr_current_rows / $mzcr_total_rows]"); say "";
}

sub compute_age_groups_sexes_offsets {
    open my $out_deaths, '>', 'outputs/deaths_offset_2.csv';
    say $out_deaths "year,week,age_group,sex,mzcr_deaths,eurostat_deaths,mzcr_minus_eurostats";
    for my $year (sort {$a<=>$b} keys %mzcr_deaths) {
        for my $week (sort {$a<=>$b} keys %{$mzcr_deaths{$year}}) {
            if ($year == 2020) { next if $week < 10; }
            elsif ($year == 2024) { next; }
            for my $age_group (@age_groups) {
                for my $sex (@sexes) {
                    my $eu  = $eurostat_deaths{$year}->{$week}->{$age_group}->{$sex} // 0;
                    my $mz  = $mzcr_deaths{$year}->{$week}->{$age_group}->{$sex}     // 0;
                    my $off = $mz - $eu;
                    if ($off > 0) {
                        $total_deaths_extra += $off;
                    } else {
                        $weeks_requiring_imputation{$year}->{$week}->{$age_group}->{$sex} = {
                            eurostat_deaths => $eu,
                            mzcr_deaths     => $mz,
                            missing_in_mzcr => abs($off),
                        };
                        $total_deaths_missing += $off;
                    }
                    say $out_deaths "$year,$week,$age_group,$sex,$mz,$eu,$off";
                }
            }
        }
    }
    close $out_deaths;
}

sub load_no_yob_deaths {
    my $mzcr_origin_total_rows = 12125969;
    my ($cpt_origin, $mzcr_origin_current_rows, $unknown_yob_deaths) = (0, 0, 0);
    my ($mzcr_origin_csv, $mzcr_origin_fh, $mzcr_origin_headers, $mzcr_origin_idx) = open_csv_reader($mzcr_origin_file);
    while (my $row = $mzcr_origin_csv->getline_hr($mzcr_origin_fh)) {
        $mzcr_origin_current_rows++; $cpt_origin++;
        if ($cpt_origin == 1000) { $cpt_origin = 0; STDOUT->printflush("\rParsing MZCR_origin - [$mzcr_origin_current_rows / $mzcr_origin_total_rows]"); }
        my $id                 = $row->{'ID'}                 // die;
        my $week_date_of_death = $row->{'week_date_of_death'} // die;
        my $year_of_birth_end  = $row->{'year_of_birth_end'};
        my $gender             = $row->{'gender'};
        my $sex = $gender && $gender == 1 ? 'M' : $gender && $gender == 2 ? 'F' : 'U';
        if ($week_date_of_death) {
            my ($death_year, $death_week) = ymd_to_iso_year_week($week_date_of_death);
            $no_yob_deaths{$death_year}->{$death_week}->{$sex}->{'total_deaths'}++;
            if (!$year_of_birth_end) {
                $unknown_yob_deaths++;
                $no_yob_deaths{$death_year}->{$death_week}->{$sex}->{'total_deaths_with_no_dob'}++;
                $no_yob_ids{$id} = {
                    week_date_of_death => $week_date_of_death,
                    death_year         => $death_year,
                    death_week         => $death_week,
                    sex                => $sex,
                };
            }
        }
    }
    $mzcr_origin_csv->eof or die "CSV error in $mzcr_origin_file: " . $mzcr_origin_csv->error_diag;
    close $mzcr_origin_fh or warn "Couldn't close $mzcr_origin_file: $!";
    STDOUT->printflush("\rParsing MZCR_origin - [$mzcr_origin_current_rows / $mzcr_origin_total_rows]"); say "";
    say "-" x 50;
    say "unknown_yob_deaths       : $unknown_yob_deaths";
    say "-" x 50;
}

# ---- apportion with caps (largest remainder, integer) ----
sub apportion_with_caps {
    my ($total, $items) = @_; # [ [ key, weight, cap ], ... ]
    my %alloc = map { $_->[0] => 0 } @$items;
    return %alloc if $total <= 0 || !@$items;
    my @rows = grep { $_->[2] > 0 } @$items;
    return %alloc unless @rows;

    my $sum_w = 0; $sum_w += $_->[1] for @rows;
    if ($sum_w <= 0) {
        my $rem = $total;
        for my $r (@rows) {
            last if $rem <= 0;
            my $take = $r->[2] < $rem ? $r->[2] : $rem;
            $alloc{$r->[0]} = $take; $rem -= $take;
        }
        return %alloc;
    }

    my $allocated = 0;
    my @quota_rows;
    for my $r (@rows) {
        my ($key,$w,$cap) = @$r;
        my $q = $total * ($w / $sum_w);
        my $f = int($q);
        $f = $cap if $f > $cap;
        push @quota_rows, [$key,$w,$cap,$q,$f,($q-$f)];
        $allocated += $f;
    }
    my $remain = $total - $allocated;
    if ($remain > 0) {
        for my $row (sort { $b->[5] <=> $a->[5] } @quota_rows) {
            last if $remain <= 0;
            next if $row->[4] >= $row->[2];
            $row->[4]++; $remain--;
        }
    } elsif ($remain < 0) {
        for my $row (sort { $a->[5] <=> $b->[5] } @quota_rows) {
            last if $remain >= 0;
            next if $row->[4] <= 0;
            $row->[4]--; $remain++;
        }
    }
    %alloc = map { $_->[0] => $_->[4] } @quota_rows;
    return %alloc;
}

sub calculate_weekly_imputations {
    for my $year (sort { $a <=> $b } keys %no_yob_deaths) {
        for my $week (sort { $a <=> $b } keys %{ $no_yob_deaths{$year} }) {
            next unless exists $weeks_requiring_imputation{$year}->{$week};
            my $avail_U = $no_yob_deaths{$year}->{$week}->{'U'}->{'total_deaths_with_no_dob'} // 0;
            my %avail_same = (
                M => ($no_yob_deaths{$year}->{$week}->{'M'}->{'total_deaths_with_no_dob'} // 0),
                F => ($no_yob_deaths{$year}->{$week}->{'F'}->{'total_deaths_with_no_dob'} // 0),
            );

            my %bin_need;   # "age|sex" => need
            my %bin_weight; # "age|sex" => eurostat deaths weight
            my ($weekly_total_eurostat_deaths, $weekly_total_to_attribute) = (0,0);

            for my $age_group (keys %{ $weeks_requiring_imputation{$year}->{$week} }) {
                for my $sex (keys %{ $weeks_requiring_imputation{$year}->{$week}->{$age_group} }) {
                    my $need = $weeks_requiring_imputation{$year}->{$week}->{$age_group}->{$sex}->{'missing_in_mzcr'} // 0;
                    next unless $need > 0;
                    my $w    = $weeks_requiring_imputation{$year}->{$week}->{$age_group}->{$sex}->{'eurostat_deaths'} // 0;
                    my $k = "$age_group|$sex";
                    $bin_need{$k}   = $need;
                    $bin_weight{$k} = $w;
                    $weekly_total_eurostat_deaths += $w;
                    $weekly_total_to_attribute    += $need;
                }
            }
            next if $weekly_total_to_attribute <= 0;

            $weekly_deaths_imputations{$year}->{$week} = {
                'weekly_total_eurostat_deaths'            => $weekly_total_eurostat_deaths,
                'weekly_total_known_yob_mzcr_deaths'      => 0,
                'weekly_total_to_attribute'               => $weekly_total_to_attribute,
                'weekly_unknown_sex_unknown_available'    => $avail_U,
                'weekly_total_same_sex_unknown_available' => ($avail_same{M} + $avail_same{F}),
                'weekly_overall_unknown_yob_available'    => ($avail_U + $avail_same{M} + $avail_same{F}),
                'age_groups'                              => {},
            };

            # Stage A: same-sex pool
            for my $sex (qw(M F)) {
                my $avail = $avail_same{$sex} // 0;
                next if $avail <= 0;
                my @items;
                for my $k (grep { /\|$sex\z/ } keys %bin_need) {
                    my ($ag) = split /\|/, $k;
                    my $cap  = $bin_need{$k};
                    my $w    = $bin_weight{$k} // 0;
                    push @items, [ $k, $w, $cap ] if $cap > 0;
                }
                next unless @items;
                my %alloc = apportion_with_caps($avail, \@items);
                for my $k (keys %alloc) {
                    my ($ag,$sx) = split /\|/, $k;
                    my $x = $alloc{$k} // 0; next if $x <= 0;
                    $bin_need{$k} -= $x;
                    $weekly_deaths_imputations{$year}->{$week}->{'age_groups'}->{$ag}->{$sx} //= {};
                    $weekly_deaths_imputations{$year}->{$week}->{'age_groups'}->{$ag}->{$sx}->{'same_sex_unknown_available'} = $x;
                    $weekly_deaths_imputations{$year}->{$week}->{'age_groups'}->{$ag}->{$sx}->{'missing_in_mzcr'}  //= ($x + $bin_need{$k});
                    $weekly_deaths_imputations{$year}->{$week}->{'age_groups'}->{$ag}->{$sx}->{'eurostat_deaths'}  //= ($bin_weight{$k} // 0);
                }
            }

            # Stage B: unknown-sex pool
            if ($avail_U > 0) {
                my @items;
                for my $k (keys %bin_need) {
                    my ($ag,$sx) = split /\|/, $k;
                    my $cap = $bin_need{$k}; next unless $cap > 0;
                    my $w   = $bin_weight{$k} // 0;
                    push @items, [ $k, $w, $cap ];
                }
                if (@items) {
                    my %alloc = apportion_with_caps($avail_U, \@items);
                    for my $k (keys %alloc) {
                        my ($ag,$sx) = split /\|/, $k;
                        my $x = $alloc{$k} // 0; next if $x <= 0;
                        $bin_need{$k} -= $x;
                        $weekly_deaths_imputations{$year}->{$week}->{'age_groups'}->{$ag}->{$sx} //= {};
                        $weekly_deaths_imputations{$year}->{$week}->{'age_groups'}->{$ag}->{$sx}->{'unknown_sex_to_attrib'} = $x;
                        $weekly_deaths_imputations{$year}->{$week}->{'age_groups'}->{$ag}->{$sx}->{'missing_in_mzcr'}  //= ($x + $bin_need{$k});
                        $weekly_deaths_imputations{$year}->{$week}->{'age_groups'}->{$ag}->{$sx}->{'eurostat_deaths'}  //= ($bin_weight{$k} // 0);
                    }
                }
            }

            # Ensure baseline fields exist
            for my $k (keys %bin_need) {
                my ($ag,$sx) = split /\|/, $k;
                $weekly_deaths_imputations{$year}->{$week}->{'age_groups'}->{$ag}->{$sx} //= {};
                $weekly_deaths_imputations{$year}->{$week}->{'age_groups'}->{$ag}->{$sx}->{'missing_in_mzcr'}  //= $weeks_requiring_imputation{$year}->{$week}->{$ag}->{$sx}->{'missing_in_mzcr'} // 0;
                $weekly_deaths_imputations{$year}->{$week}->{'age_groups'}->{$ag}->{$sx}->{'eurostat_deaths'}  //= $weeks_requiring_imputation{$year}->{$week}->{$ag}->{$sx}->{'eurostat_deaths'} // 0;
                $weekly_deaths_imputations{$year}->{$week}->{'age_groups'}->{$ag}->{$sx}->{'unknown_sex_to_attrib'} //= 0;
                $weekly_deaths_imputations{$year}->{$week}->{'age_groups'}->{$ag}->{$sx}->{'same_sex_unknown_available'} //= 0;
            }
        }
    }
    say "-" x 50;
}

sub print_imputation_details {
    my $out_path = 'outputs/layer_3_deaths_yob_imputation_by_by_ages_and_sexes.csv';
    make_path('outputs') unless -d 'outputs';
    open my $fh, '>:encoding(UTF-8)', $out_path or die "Can't write $out_path: $!";
    $csv->print($fh, [qw(
        year week age_group sex
        weekly_total_eurostat_deaths
        weekly_total_known_yob_mzcr_deaths
        weekly_total_to_attribute
        weekly_unknown_sex_unknown_available
        weekly_available_pct
        weekly_total_same_sex_unknown_available
        weekly_overall_unknown_yob_available
        missing_in_mzcr
        eurostat_deaths
        eurostat_age_group_share_of_total_deaths
        deaths_to_attrib_on_ag_sex
        unknown_sex_to_attrib
        same_sex_unknown_available
    )]);

    for my $year (sort { $a <=> $b } keys %weekly_deaths_imputations) {
        for my $week (sort { $a <=> $b } keys %{ $weekly_deaths_imputations{$year} }) {
            my $W = $weekly_deaths_imputations{$year}->{$week};
            my $groups = $W->{'age_groups'} // {};
            for my $age_group (sort {
                    my ($af,$at)=split/-/,$a; my ($bf,$bt)=split/-/,$b; ($af<=>$bf)||($at<=>$bt)
                } keys %$groups) {
                for my $sex (sort keys %{ $groups->{$age_group} }) {
                    my $g = $groups->{$age_group}->{$sex};
                    $csv->print($fh, [
                        $year, $week, $age_group, $sex,
                        ($W->{'weekly_total_eurostat_deaths'}            // 0),
                        ($W->{'weekly_total_known_yob_mzcr_deaths'}      // 0),
                        ($W->{'weekly_total_to_attribute'}               // 0),
                        ($W->{'weekly_unknown_sex_unknown_available'}    // 0),
                        ($W->{'weekly_available_pct'}                    // 0),
                        ($W->{'weekly_total_same_sex_unknown_available'} // 0),
                        ($W->{'weekly_overall_unknown_yob_available'}    // 0),
                        ($g->{'missing_in_mzcr'}                          // 0),
                        ($g->{'eurostat_deaths'}                          // 0),
                        ($g->{'eurostat_age_group_share_of_total_deaths'} // 0),
                        ($g->{'deaths_to_attrib_on_ag_sex'}               // 0),
                        ($g->{'unknown_sex_to_attrib'}                    // 0),
                        ($g->{'same_sex_unknown_available'}               // 0),
                    ]);
                }
            }
        }
    }
    close $fh or warn "Couldn't close $out_path: $!";
    say "Wrote $out_path";
}

sub print_known_dob_deaths {
    my $total_ids = keys %known_yob_data;
    my $current = 0;
    for my $id (sort keys %known_yob_data) {
        $current++;
        my $sex                = $known_yob_data{$id}->{'sex'}                // die;
        my $death_year         = $known_yob_data{$id}->{'death_year'}         // die;
        my $death_week         = $known_yob_data{$id}->{'death_week'}         // die;
        my $age_group          = $known_yob_data{$id}->{'age_group'}          // die;
        my $week_date_of_death = $known_yob_data{$id}->{'week_date_of_death'} // die;
        my $year_of_birth_end  = $known_yob_data{$id}->{'year_of_birth_end'}  // die;
        my $age_at_death       = $known_yob_data{$id}->{'age_at_death'}       // die;
        STDOUT->printflush("\rPrinting known DOB/Gender who died - MZCR - [$current / $total_ids]");
        say $out_imput "$id,$sex,$year_of_birth_end,$week_date_of_death,$age_at_death,$death_year,$death_week,$age_group";
    }
    say "";
}

# ---------- Stage A/B: per-ID use of planned bins; collect leftovers ----------
my %leftovers;  # {year}{week}{sex} => [ ids ... ]
sub impute_to_unknown_yob_sex {
    my %quick_check = ();
    my $total_ids = keys %no_yob_ids;
    my ($current,$non_attributed,$total_attributed) = (0,0,0);

    for my $id (sort keys %no_yob_ids) {
        $current++;
        my $week_date_of_death = $no_yob_ids{$id}->{'week_date_of_death'} // die;
        my $death_year = $no_yob_ids{$id}->{'death_year'} // die;
        my $death_week = $no_yob_ids{$id}->{'death_week'} // die;
        my $sex        = $no_yob_ids{$id}->{'sex'}        // die;
        STDOUT->printflush("\rImputing unknown DOB/Gender who died - MZCR - [$current / $total_ids]");

        my $subject_attributed = 0;
        my $age_group;

        if ($sex ne 'U') {
            my $groups = $weekly_deaths_imputations{$death_year}->{$death_week}->{'age_groups'} // {};
            for my $ag (sort {
                    my ($af,$at)=split/-/,$a; my ($bf,$bt)=split/-/,$b; ($af<=>$bf)||($at<=>$bt)
                } keys %$groups) {
                for my $sx (sort keys %{ $groups->{$ag} }) {
                    next unless $sx eq $sex;
                    next unless $groups->{$ag}->{$sx}->{'same_sex_unknown_available'};
                    my $avail = $groups->{$ag}->{$sx}->{'same_sex_unknown_available'} // die;
                    next unless $avail;
                    $age_group = $ag; $subject_attributed = 1;
                    $avail -= 1;
                    $weekly_deaths_imputations{$death_year}->{$death_week}->{'age_groups'}->{$ag}->{$sx}->{'same_sex_unknown_available'} = $avail;
                    last;
                }
                last if $subject_attributed;
            }
        } else {
            my $groups = $weekly_deaths_imputations{$death_year}->{$death_week}->{'age_groups'} // {};
            for my $ag (sort {
                    my ($af,$at)=split/-/,$a; my ($bf,$bt)=split/-/,$b; ($af<=>$bf)||($at<=>$bt)
                } keys %$groups) {
                for my $sx (sort keys %{ $groups->{$ag} }) {
                    next unless $groups->{$ag}->{$sx}->{'unknown_sex_to_attrib'};
                    my $avail = $groups->{$ag}->{$sx}->{'unknown_sex_to_attrib'} // die;
                    next unless $avail;
                    $age_group = $ag; $sex = $sx; $subject_attributed = 1;
                    $avail -= 1;
                    $weekly_deaths_imputations{$death_year}->{$death_week}->{'age_groups'}->{$ag}->{$sx}->{'unknown_sex_to_attrib'} = $avail;
                    last;
                }
                last if $subject_attributed;
            }
        }

        unless ($age_group) {
            # collect for Stage C fallback (Eurostat distribution)
            push @{ $leftovers{$death_year}->{$death_week}->{$sex} }, $id;
            $non_attributed++;
            next;
        }

        my ($from_year)        = split '-', $age_group;
        my $year_of_birth_end  = $death_year - $from_year;
        my $age_at_death       = $death_year - $year_of_birth_end; # == $from_year
        $quick_check{$age_group} = $year_of_birth_end;
        say $out_imput "$id,$sex,$year_of_birth_end,$week_date_of_death,$age_at_death,$death_year,$death_week,$age_group";
        $total_attributed++;
    }
    say "";
    p%quick_check;
    say "-" x 50;
    say "non_attributed (before eurostat-fallback) : $non_attributed";
    say "total_attributed (stage A/B)             : $total_attributed";
    say "-" x 50;

    # ---------- Stage C: redistribute leftovers by Eurostat age (and sex) ----------
    my $redistributed = redistribute_leftovers_by_eurostat(\%leftovers);
    say "-" x 50;
    say "redistributed by Eurostat fallback        : $redistributed";
    say "still_non_attributed (after fallback)    : " . ($non_attributed - $redistributed);
    say "-" x 50;
}

# ---------- Stage C fallback ----------
sub redistribute_leftovers_by_eurostat {
    my ($lf_ref) = @_;
    my $assigned = 0;

    for my $year (sort {$a<=>$b} keys %$lf_ref) {
        for my $week (sort {$a<=>$b} keys %{$lf_ref->{$year}}) {

            # Gather lists
            my @L_M = @{ $lf_ref->{$year}->{$week}->{'M'} // [] };
            my @L_F = @{ $lf_ref->{$year}->{$week}->{'F'} // [] };
            my @L_U = @{ $lf_ref->{$year}->{$week}->{'U'} // [] };

            # Split U into M/F by Eurostat totals
            my $U_total = scalar @L_U;
            if ($U_total > 0) {
                my $sumM = 0; my $sumF = 0;
                for my $ag (@age_groups) {
                    $sumM += $eurostat_deaths{$year}->{$week}->{$ag}->{'M'} // 0;
                    $sumF += $eurostat_deaths{$year}->{$week}->{$ag}->{'F'} // 0;
                }
                # use apportion_with_caps to split U across M,F
                my @sex_items = (
                    [ 'M', ($sumM > 0 ? $sumM : 1), $U_total ],
                    [ 'F', ($sumF > 0 ? $sumF : 1), $U_total ],
                );
                my %u_split = apportion_with_caps($U_total, \@sex_items);
                my $to_M = $u_split{'M'} // 0; my $to_F = $u_split{'F'} // 0;

                # Move the first $to_M/$to_F U-ids to M/F buckets
                while ($to_M-- > 0 && @L_U) { push @L_M, shift @L_U; }
                while ($to_F-- > 0 && @L_U) { push @L_F, shift @L_U; }
                # if any residual due to rounding, dump to whichever bucket has fewer
                while (@L_U) {
                    if (@L_M <= @L_F) { push @L_M, shift @L_U; } else { push @L_F, shift @L_U; }
                }
            }

            # Per-sex allocation across age groups by Eurostat weights
            for my $sx (qw(M F)) {
                my @L = $sx eq 'M' ? @L_M : @L_F;
                my $N = scalar @L; next unless $N > 0;

                my $sumW = 0;
                my @items;
                for my $ag (@age_groups) {
                    my $w = $eurostat_deaths{$year}->{$week}->{$ag}->{$sx} // 0;
                    $sumW += $w;
                    push @items, [ $ag, $w, $N ];
                }
                # If Eurostat has no weights this week for this sex, use equal weights
                if ($sumW <= 0) {
                    @items = map { [ $_, 1, $N ] } @age_groups;
                }

                my %alloc = apportion_with_caps($N, \@items);

                # Now write out each ID for the allocated age group
                for my $ag (sort {
                        my ($af,$at)=split/-/,$a; my ($bf,$bt)=split/-/,$b; ($af<=>$bf)||($at<=>$bt)
                    } keys %alloc) {
                    my $take = $alloc{$ag} // 0;
                    next if $take <= 0;
                    for (1..$take) {
                        my $id = shift @L;
                        last unless defined $id;

                        my $week_date_of_death = $no_yob_ids{$id}->{'week_date_of_death'} // die;
                        my $death_year         = $year;
                        my $death_week         = $week;
                        my ($from_year)        = split '-', $ag;
                        my $year_of_birth_end  = $death_year - $from_year;
                        my $age_at_death       = $death_year - $year_of_birth_end;

                        say $out_imput "$id,$sx,$year_of_birth_end,$week_date_of_death,$age_at_death,$death_year,$death_week,$ag";
                        $assigned++;
                    }
                }
            }
        }
    }
    return $assigned;
}
