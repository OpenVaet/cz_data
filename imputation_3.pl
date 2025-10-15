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
my %eurostat_deaths      = ();
my %mzcr_deaths          = ();
my %data_to_attribute    = ();
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

# --- Offset in deaths when sex & YOB is known ---
my @age_groups = ("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-999");
my @sexes = ('M', 'F');
compute_age_groups_sexes_offsets();

# --- Rendering layer 2 statistics ---
$total_deaths_extra   = abs($total_deaths_extra);
$total_deaths_missing = abs($total_deaths_missing);
say "-" x 50;
say "----- Post-imputation 2 Recap. -----";
say "-" x 50;
say "total_deaths_extra       : $total_deaths_extra";
say "total_deaths_missing     : $total_deaths_missing";
say "-" x 50;

# --- MZCR Original file ---
my %no_yob_deaths   = ();
my %no_yob_ids = ();
load_no_yob_deaths();

# --- Analyses sex & YOB to attribute weekly ---
my %weekly_deaths_imputations = ();
calculate_weekly_imputations();

# --- Outputting the imputation details ---
# Prefer Text::CSV_XS, fall back to Text::CSV
print_imputation_details();

# --- Outputting the already imputed known deaths ---
open my $out_imput, '>', 'outputs/imputation_layer_3.csv';
say $out_imput "id,sex,year_of_birth_end,week_date_of_death,age_at_death,death_year,death_week,age_group";
print_known_dob_deaths();

# --- Attributing to the unknown YOB the imputed ones. ---
impute_to_unknown_yob_sex();

close $out_imput;
say "";

# --- helpers ---
sub ymd_to_iso_year_week {
    my ($ymd) = @_;
    die "Bad date: $ymd" unless defined $ymd && $ymd =~ /\A\d{4}-\d{2}-\d{2}\z/;

    my $t = Time::Piece->strptime($ymd, "%Y-%m-%d");

    # ISO week number (01..53) -> normalize to integer to avoid "01" vs "1" key mismatch
    my $iso_week = 0 + $t->strftime("%V");

    # ISO weekday (1=Mon..7=Sun)
    my $iso_wday = 0 + $t->strftime("%u");

    # ISO year is the year of the Thursday in this week
    my $thu      = $t + (4 - $iso_wday) * ONE_DAY;
    my $iso_year = 0 + $thu->strftime("%Y");

    return ($iso_year, $iso_week);
}

sub open_csv_reader {
    my ($path) = @_;

    open my $fh, "<:encoding(UTF-8)", $path
        or die "Can't open $path: $!";

    my $header_line = <$fh>;
    die "Empty CSV: $path" unless defined $header_line;

    # Strip UTF-8 BOM if present
    $header_line =~ s/^\x{FEFF}//;
    $header_line =~ s/^\xEF\xBB\xBF//;

    # Detect separator (comma/semicolon/tab)
    my $sep_char = ',';
    if ($header_line =~ /;/ && $header_line !~ /,/) {
        $sep_char = ';';
    } elsif ($header_line =~ /\t/ && $header_line !~ /,|;/) {
        $sep_char = "\t";
    }

    my $csv = Text::CSV_XS->new({
        binary              => 1,
        auto_diag           => 1,
        sep_char            => $sep_char,
        quote_char          => '"',
        escape_char         => '"',
        allow_loose_quotes  => 1,
        allow_loose_escapes => 1,
        blank_is_undef      => 0,
    }) or die "Text::CSV_XS->new failed: " . Text::CSV_XS->error_diag;

    # Parse header row and set column names
    $csv->parse($header_line) or die "Header parse failed for $path: " . $csv->error_diag;
    my @headers = map {
        my $h = $_ // '';
        $h =~ s/^\s+|\s+$//g;
        $h;
    } $csv->fields;
    $csv->column_names(\@headers);

    # Optional: index map if you prefer working with array rows
    my %col_idx;
    for (my $i = 0; $i < @headers; $i++) {
        $col_idx{$headers[$i]} = $i;
    }

    return ($csv, $fh, \@headers, \%col_idx);
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

sub load_eurostat_deaths {
	# --- Eurostat deaths file ---
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
		my $sex_code;
		if ($sex eq 'F: Females') {
			$sex_code = 'F';
		} elsif ($sex eq 'M: Males') {
			$sex_code = 'M';
		} else {
			die;
		}
		my ($year, $week) = split '-', $week_code;
		my ($from_year, $to_year);
		if ($age eq "UNK: Unknown") {
			die if $value;
			next;
		} elsif ($age eq "Y_LT5: Less than 5 years") {
			($from_year, $to_year) = (0, 4);
		} elsif ($age eq "Y5-9: From 5 to 9 years") {
			($from_year, $to_year) = (5, 9);
		} elsif ($age eq "Y10-14: From 10 to 14 years") {
			($from_year, $to_year) = (10, 14);
		} elsif ($age eq "Y15-19: From 15 to 19 years") {
			($from_year, $to_year) = (15, 19);
		} elsif ($age eq "Y20-24: From 20 to 24 years") {
			($from_year, $to_year) = (20, 24);
		} elsif ($age eq "Y25-29: From 25 to 29 years") {
			($from_year, $to_year) = (25, 29);
		} elsif ($age eq "Y30-34: From 30 to 34 years") {
			($from_year, $to_year) = (30, 34);
		} elsif ($age eq "Y35-39: From 35 to 39 years") {
			($from_year, $to_year) = (35, 39);
		} elsif ($age eq "Y40-44: From 40 to 44 years") {
			($from_year, $to_year) = (40, 44);
		} elsif ($age eq "Y45-49: From 45 to 49 years") {
			($from_year, $to_year) = (45, 49);
		} elsif ($age eq "Y50-54: From 50 to 54 years") {
			($from_year, $to_year) = (50, 54);
		} elsif ($age eq "Y55-59: From 55 to 59 years") {
			($from_year, $to_year) = (55, 59);
		} elsif ($age eq "Y60-64: From 60 to 64 years") {
			($from_year, $to_year) = (60, 64);
		} elsif ($age eq "Y65-69: From 65 to 69 years") {
			($from_year, $to_year) = (65, 69);
		} elsif ($age eq "Y70-74: From 70 to 74 years") {
			($from_year, $to_year) = (70, 74);
		} elsif ($age eq "Y75-79: From 75 to 79 years") {
			($from_year, $to_year) = (75, 79);
		} elsif ($age eq "Y80-84: From 80 to 84 years") {
			($from_year, $to_year) = (80, 84);
		} elsif ($age eq "Y85-89: From 85 to 89 years") {
			($from_year, $to_year) = (85, 89);
		} elsif ($age eq "Y_GE90: 90 years or over") {
			($from_year, $to_year) = (90, 999);
		} else {
			die;
		}
		my $age_group = "$from_year-$to_year";
		$week =~ s/W//;
		die unless looks_like_number($week);
		$year = 0 + $year;
		$week = 0 + $week;
		$eurostat_deaths{$year}->{$week}->{$age_group}->{$sex_code} += $value;
	}
	$euro_csv->eof or die "CSV error in $eurostat_deaths_file: " . $euro_csv->error_diag;
	close $euro_fh or warn "Couldn't close $eurostat_deaths_file: $!";
}

sub load_mzcr_known_yob_deaths {
	# --- MZCR deaths with known YOB file ---
	my $mzcr_total_rows = 493208;
	my ($cpt, $mzcr_current_rows) = (0, 0);
	my ($mzcr_csv, $mzcr_fh, $mzcr_headers, $mzcr_idx) = open_csv_reader($mzcr_file);
	while (my $row = $mzcr_csv->getline_hr($mzcr_fh)) {
		$mzcr_current_rows++;
		$cpt++;
		if ($cpt == 1000) {
			$cpt = 0;
			STDOUT->printflush("\rParsing MZCR - [$mzcr_current_rows / $mzcr_total_rows]");
		}
		my $week_date_of_death        = $row->{'week_date_of_death'} // die;
		my $year_of_birth_end         = $row->{'year_of_birth_end'}  // die;
		my $age_at_death              = $row->{'age_at_death'}       // die;
		my $sex                       = $row->{'sex'}                // die;
		my $id                        = $row->{'id'}                 // die;
		my ($death_year, $death_week) = ymd_to_iso_year_week($week_date_of_death);
		my ($from_year, $to_year)     = from_year_to_year_from_age($age_at_death);
		my $age_group                 = "$from_year-$to_year";
		$mzcr_deaths{$death_year}->{$death_week}->{$age_group}->{$sex}++;
		$known_yob_data{$id}->{'week_date_of_death'} = $week_date_of_death;
		$known_yob_data{$id}->{'year_of_birth_end'}  = $year_of_birth_end;
		$known_yob_data{$id}->{'age_at_death'}       = $age_at_death;
		$known_yob_data{$id}->{'sex'}                = $sex;
		$known_yob_data{$id}->{'death_year'}         = $death_year;
		$known_yob_data{$id}->{'death_week'}         = $death_week;
		$known_yob_data{$id}->{'age_group'}          = $age_group;
	}
	$mzcr_csv->eof or die "CSV error in $mzcr_file: " . $mzcr_csv->error_diag;
	close $mzcr_fh or warn "Couldn't close $mzcr_file: $!";
	STDOUT->printflush("\rParsing MZCR - [$mzcr_current_rows / $mzcr_total_rows]");
	say "";
}

sub compute_age_groups_sexes_offsets {
	open my $out_deaths, '>', 'outputs/deaths_offset_2.csv';
	say $out_deaths "year,week,age_group,sex,mzcr_deaths,eurostat_deaths,mzcr_minus_eurostats";
	for my $year (sort{$a <=> $b} keys %mzcr_deaths) {
		for my $week (sort{$a <=> $b} keys %{$mzcr_deaths{$year}}) {
			if ($year == 2020) { next if $week < 10; }
			elsif ($year == 2024) { next; }
			for my $age_group (@age_groups) {
				for my $sex (@sexes) {
					my $eurostat_deaths      = $eurostat_deaths{$year}->{$week}->{$age_group}->{$sex} // 0;
					my $mzcr_deaths          = $mzcr_deaths{$year}->{$week}->{$age_group}->{$sex}     // 0;
					my $mzcr_minus_eurostats = $mzcr_deaths - $eurostat_deaths;
					if ($mzcr_minus_eurostats > 0) {
						$total_deaths_extra += $mzcr_minus_eurostats;
						say $out_deaths "$year,$week,$age_group,$sex,$mzcr_deaths,$eurostat_deaths,$mzcr_minus_eurostats";
					} else {
						$weeks_requiring_imputation{$year}->{$week}->{$age_group}->{$sex}->{'eurostat_deaths'} = $eurostat_deaths;
						$weeks_requiring_imputation{$year}->{$week}->{$age_group}->{$sex}->{'mzcr_deaths'}     = $mzcr_deaths;
						$weeks_requiring_imputation{$year}->{$week}->{$age_group}->{$sex}->{'missing_in_mzcr'} = abs($mzcr_minus_eurostats);
						$total_deaths_missing += $mzcr_minus_eurostats;
						say $out_deaths "$year,$week,$age_group,$sex,$mzcr_deaths,$eurostat_deaths,$mzcr_minus_eurostats";
					}
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
		$mzcr_origin_current_rows++;
		$cpt_origin++;
		if ($cpt_origin == 1000) {
			$cpt_origin = 0;
			STDOUT->printflush("\rParsing MZCR_origin - [$mzcr_origin_current_rows / $mzcr_origin_total_rows]");
		}
		my $id                 = $row->{'ID'}                 // die;
		my $week_date_of_death = $row->{'week_date_of_death'} // die;
		my $year_of_birth_end  = $row->{'year_of_birth_end'};
		my $age_at_death       = $row->{'age_at_death'};
		my $gender             = $row->{'gender'};
		my $sex;
		if ($gender eq 1) {
			$sex = 'M';
		} elsif ($gender eq 2) {
			$sex = 'F';
		} else {
			$sex = 'U';
		}
		if ($week_date_of_death) {
			my ($death_year, $death_week) = ymd_to_iso_year_week($week_date_of_death);
			$no_yob_deaths{$death_year}->{$death_week}->{$sex}->{'total_deaths'}++;
			if (!$year_of_birth_end) {
				$unknown_yob_deaths++;
				$no_yob_deaths{$death_year}->{$death_week}->{$sex}->{'total_deaths_with_no_dob'}++;
				$no_yob_ids{$id}->{'week_date_of_death'} = $week_date_of_death;
				$no_yob_ids{$id}->{'death_year'}         = $death_year;
				$no_yob_ids{$id}->{'death_week'}         = $death_week;
				$no_yob_ids{$id}->{'sex'}                = $sex;
			}
		}
	}
	$mzcr_origin_csv->eof or die "CSV error in $mzcr_origin_file: " . $mzcr_origin_csv->error_diag;
	close $mzcr_origin_fh or warn "Couldn't close $mzcr_origin_file: $!";
	STDOUT->printflush("\rParsing MZCR_origin - [$mzcr_origin_current_rows / $mzcr_origin_total_rows]");
	say "";
	say "-" x 50;
	say "unknown_yob_deaths       : $unknown_yob_deaths";
	say "-" x 50;
}

# ---- helper: largest-remainder apportionment with caps (integers) ----
sub apportion_with_caps {
    my ($total, $items) = @_; 
    # $items = [ [ key, weight, cap ], ... ]
    my %alloc = map { $_->[0] => 0 } @$items;
    return %alloc if $total <= 0 || !@$items;

    # keep only items with positive cap
    my @rows = grep { $_->[2] > 0 } @$items;
    return %alloc unless @rows;

    my $sum_w = 0;
    $sum_w += $_->[1] for @rows;
    # if all weights are zero, just fill in order up to caps
    if ($sum_w <= 0) {
        my $rem = $total;
        for my $r (@rows) {
            last if $rem <= 0;
            my $take = $r->[2] < $rem ? $r->[2] : $rem;
            $alloc{$r->[0]} = $take;
            $rem -= $take;
        }
        return %alloc;
    }

    # quotas, floors (capped), remainders
    my $allocated = 0;
    my @quota_rows;
    for my $r (@rows) {
        my ($key, $w, $cap) = @$r;
        my $q = $total * ($w / $sum_w);
        my $f = int($q);
        $f = $cap if $f > $cap;         # respect cap
        push @quota_rows, [$key, $w, $cap, $q, $f, ($q - $f)];
        $allocated += $f;
    }

    my $remain = $total - $allocated;
    if ($remain > 0) {
        # assign remaining units by largest fractional part, still respecting caps
        for my $row (sort { $b->[5] <=> $a->[5] } @quota_rows) {
            last if $remain <= 0;
            next if $row->[4] >= $row->[2];   # at cap
            $row->[4]++; 
            $remain--;
        }
    } elsif ($remain < 0) {
        # over-allocated due to caps interaction: remove from smallest remainders first
        for my $row (sort { $a->[5] <=> $b->[5] } @quota_rows) {
            last if $remain >= 0;
            next if $row->[4] <= 0;
            $row->[4]--;
            $remain++;
        }
    }

    %alloc = map { $_->[0] => $_->[4] } @quota_rows;
    return %alloc;
}

sub calculate_weekly_imputations {
    for my $year (sort { $a <=> $b } keys %no_yob_deaths) {
        for my $week (sort { $a <=> $b } keys %{ $no_yob_deaths{$year} }) {
            next unless exists $weeks_requiring_imputation{$year}->{$week};
            # pools available this week
            my $avail_U = $no_yob_deaths{$year}->{$week}->{'U'}->{'total_deaths_with_no_dob'} // 0;
            my %avail_same = (
                M => ($no_yob_deaths{$year}->{$week}->{'M'}->{'total_deaths_with_no_dob'} // 0),
                F => ($no_yob_deaths{$year}->{$week}->{'F'}->{'total_deaths_with_no_dob'} // 0),
            );

            # collect all bins (age_group, sex) with deficits + eurostat weights
            my %bin_need;     # key "age|sex" => remaining deficit
            my %bin_weight;   # key "age|sex" => eurostat_deaths weight
            my ($weekly_total_eurostat_deaths, $weekly_total_to_attribute) = (0, 0);

            for my $age_group (keys %{ $weeks_requiring_imputation{$year}->{$week} }) {
                for my $sex (keys %{ $weeks_requiring_imputation{$year}->{$week}->{$age_group} }) {
                    my $need   = $weeks_requiring_imputation{$year}->{$week}->{$age_group}->{$sex}->{'missing_in_mzcr'} // 0;
                    next unless $need > 0;
                    my $w      = $weeks_requiring_imputation{$year}->{$week}->{$age_group}->{$sex}->{'eurostat_deaths'} // 0;
                    my $k = "$age_group|$sex";
                    $bin_need{$k}   = $need;
                    $bin_weight{$k} = $w;
                    $weekly_total_eurostat_deaths += $w;
                    $weekly_total_to_attribute    += $need;
                }
            }
            next if $weekly_total_to_attribute <= 0;  # nothing to do this week

            # Init container
            $weekly_deaths_imputations{$year}->{$week} = {
                'weekly_total_eurostat_deaths'            => $weekly_total_eurostat_deaths,
                'weekly_total_known_yob_mzcr_deaths'      => 0,  # not needed for allocation; fill if you want
                'weekly_total_to_attribute'               => $weekly_total_to_attribute,
                'weekly_unknown_sex_unknown_available'    => $avail_U,
                'weekly_total_same_sex_unknown_available' => ($avail_same{M} + $avail_same{F}),
                'weekly_overall_unknown_yob_available'    => ($avail_U + $avail_same{M} + $avail_same{F}),
                'age_groups'                              => {},
            };

            # Stage A: SAME-SEX pool per sex (M & F)
            for my $sex (qw(M F)) {
                my $avail = $avail_same{$sex} // 0;
                next if $avail <= 0;

                # items of this sex with remaining need
                my @items;
                for my $k (grep { /\|$sex\z/ } keys %bin_need) {
                    my ($ag) = split /\|/, $k;
                    my $cap  = $bin_need{$k};
                    my $w    = $bin_weight{$k} // 0;
                    push @items, [ $k, $w, $cap ] if $cap > 0;
                }
                next unless @items;

                my %alloc = apportion_with_caps($avail, \@items);
                # apply allocation & write to structure
                for my $k (keys %alloc) {
                    my ($ag, $sx) = split /\|/, $k;
                    my $x = $alloc{$k} // 0;
                    next if $x <= 0;
                    $bin_need{$k} -= $x;

                    $weekly_deaths_imputations{$year}->{$week}->{'age_groups'}->{$ag}->{$sx} //= {};
                    # We will CONSUME this field later during assignment:
                    $weekly_deaths_imputations{$year}->{$week}->{'age_groups'}->{$ag}->{$sx}->{'same_sex_unknown_available'} = $x;

                    # also store reference values for output/debug
                    $weekly_deaths_imputations{$year}->{$week}->{'age_groups'}->{$ag}->{$sx}->{'missing_in_mzcr'}  //= ($x + $bin_need{$k});
                    $weekly_deaths_imputations{$year}->{$week}->{'age_groups'}->{$ag}->{$sx}->{'eurostat_deaths'}  //= ($bin_weight{$k} // 0);
                }
            }

            # Stage B: UNKNOWN-SEX pool across all remaining deficits
            if ($avail_U > 0) {
                my @items;
                for my $k (keys %bin_need) {
                    my ($ag, $sx) = split /\|/, $k;
                    my $cap = $bin_need{$k};
                    next unless $cap > 0;
                    my $w   = $bin_weight{$k} // 0;
                    push @items, [ $k, $w, $cap ];
                }

                if (@items) {
                    my %alloc = apportion_with_caps($avail_U, \@items);
                    for my $k (keys %alloc) {
                        my ($ag, $sx) = split /\|/, $k;
                        my $x = $alloc{$k} // 0;
                        next if $x <= 0;
                        $bin_need{$k} -= $x;

                        $weekly_deaths_imputations{$year}->{$week}->{'age_groups'}->{$ag}->{$sx} //= {};
                        $weekly_deaths_imputations{$year}->{$week}->{'age_groups'}->{$ag}->{$sx}->{'unknown_sex_to_attrib'} = $x;
                        $weekly_deaths_imputations{$year}->{$week}->{'age_groups'}->{$ag}->{$sx}->{'missing_in_mzcr'}  //= ($x + $bin_need{$k});
                        $weekly_deaths_imputations{$year}->{$week}->{'age_groups'}->{$ag}->{$sx}->{'eurostat_deaths'}  //= ($bin_weight{$k} // 0);
                    }
                }
            }

            # For completeness: for any bin that never got set above, still record baseline fields
            for my $k (keys %bin_need) {
                my ($ag, $sx) = split /\|/, $k;
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
	# --- Ensure output dir exists ---
	my $out_path = 'outputs/layer_3_deaths_yob_imputation_by_by_ages_and_sexes.csv';
	make_path('outputs') unless -d 'outputs';

	open my $fh, '>:encoding(UTF-8)', $out_path or die "Can't write $out_path: $!";
	# Header
	$csv->print($fh, [
	    'year',
	    'week',
	    'age_group',
	    'sex',
	    'weekly_total_eurostat_deaths',
	    'weekly_total_known_yob_mzcr_deaths',
	    'weekly_total_to_attribute',
	    'weekly_unknown_sex_unknown_available',
	    'weekly_available_pct',
	    'weekly_total_same_sex_unknown_available',
	    'weekly_overall_unknown_yob_available',
	    'missing_in_mzcr',
	    'eurostat_deaths',
	    'eurostat_age_group_share_of_total_deaths',
	    'deaths_to_attrib_on_ag_sex',
	    'unknown_sex_to_attrib',
	    'same_sex_unknown_available',
	]);

	for my $year (sort { $a <=> $b } keys %weekly_deaths_imputations) {
	    for my $week (sort { $a <=> $b } keys %{ $weekly_deaths_imputations{$year} }) {

	        my $weekly_total_eurostat_deaths            = $weekly_deaths_imputations{$year}->{$week}->{'weekly_total_eurostat_deaths'}            // 0;
	        my $weekly_total_known_yob_mzcr_deaths      = $weekly_deaths_imputations{$year}->{$week}->{'weekly_total_known_yob_mzcr_deaths'}      // 0;
	        my $weekly_total_to_attribute               = $weekly_deaths_imputations{$year}->{$week}->{'weekly_total_to_attribute'}               // 0;
	        my $weekly_unknown_sex_unknown_available    = $weekly_deaths_imputations{$year}->{$week}->{'weekly_unknown_sex_unknown_available'}    // 0;
	        my $weekly_available_pct                    = $weekly_deaths_imputations{$year}->{$week}->{'weekly_available_pct'}                    // 0;
	        my $weekly_total_same_sex_unknown_available = $weekly_deaths_imputations{$year}->{$week}->{'weekly_total_same_sex_unknown_available'} // 0;
	        my $weekly_overall_unknown_yob_available    = $weekly_deaths_imputations{$year}->{$week}->{'weekly_overall_unknown_yob_available'}    // 0;

	        my $groups = $weekly_deaths_imputations{$year}->{$week}->{'age_groups'} // {};
	        for my $age_group (sort {
	                my ($af,$at) = split /-/, $a;
	                my ($bf,$bt) = split /-/, $b;
	                ($af <=> $bf) || ($at <=> $bt)
	            } keys %$groups) {

	            for my $sex (sort keys %{ $groups->{$age_group} }) {
	                my $missing_in_mzcr                          = $groups->{$age_group}->{$sex}->{'missing_in_mzcr'}                          // 0;
	                my $eurostat_deaths                          = $groups->{$age_group}->{$sex}->{'eurostat_deaths'}                          // 0;
	                my $eurostat_age_group_share_of_total_deaths = $groups->{$age_group}->{$sex}->{'eurostat_age_group_share_of_total_deaths'} // 0;
	                my $deaths_to_attrib_on_ag_sex               = $groups->{$age_group}->{$sex}->{'deaths_to_attrib_on_ag_sex'}               // 0;
	                my $unknown_sex_to_attrib                    = $groups->{$age_group}->{$sex}->{'unknown_sex_to_attrib'}                    // 0;
	                my $same_sex_unknown_available               = $groups->{$age_group}->{$sex}->{'same_sex_unknown_available'}               // 0;

	                $csv->print($fh, [
						$year,
						$week,
						$age_group,
						$sex,
						$weekly_total_eurostat_deaths,
						$weekly_total_known_yob_mzcr_deaths,
						$weekly_total_to_attribute,
						$weekly_unknown_sex_unknown_available,
						$weekly_available_pct,
						$weekly_total_same_sex_unknown_available,
						$weekly_overall_unknown_yob_available,
						$missing_in_mzcr,
						$eurostat_deaths,
						$eurostat_age_group_share_of_total_deaths,
						$deaths_to_attrib_on_ag_sex,
						$unknown_sex_to_attrib,
						$same_sex_unknown_available
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

sub impute_to_unknown_yob_sex {
	my %quick_check = ();
	# p%weekly_deaths_imputations;
	my $total_ids = keys %no_yob_ids;
	my $current = 0;
	my $non_attributed = 0;
	my $total_attributed = 0;
	for my $id (sort keys %no_yob_ids) {
		$current++;
		my $week_date_of_death = $no_yob_ids{$id}->{'week_date_of_death'} // die;
		my $death_year = $no_yob_ids{$id}->{'death_year'}                 // die;
		my $death_week = $no_yob_ids{$id}->{'death_week'}                 // die;
		my $sex        = $no_yob_ids{$id}->{'sex'}                        // die;
		STDOUT->printflush("\rImputing unknown DOB/Gender who died - MZCR - [$current / $total_ids]");
		my $subject_attributed = 0;
		my $age_group;
		if ($sex ne 'U') { # If we sex isn't undetermined, we attribute it to a cohort for which an age has been attributed.
	        my $groups = $weekly_deaths_imputations{$death_year}->{$death_week}->{'age_groups'} // {};
	        for my $ag (sort {
	                my ($af,$at) = split /-/, $a;
	                my ($bf,$bt) = split /-/, $b;
	                ($af <=> $bf) || ($at <=> $bt)
	            } keys %$groups) {

	            for my $sx (sort keys %{ $groups->{$ag} }) {
	            	next unless $sx eq $sex;
	            	next unless $groups->{$ag}->{$sx}->{'same_sex_unknown_available'};
	            	my $same_sex_unknown_available = $groups->{$ag}->{$sx}->{'same_sex_unknown_available'} // die;
	            	next unless $same_sex_unknown_available;

	            	# Attributes the target to this cohort.
	            	$age_group = $ag;
	            	$subject_attributed = 1;
	            	$same_sex_unknown_available = $same_sex_unknown_available - 1;
            		$weekly_deaths_imputations{$death_year}->{$death_week}->{'age_groups'}->{$ag}->{$sx}->{'same_sex_unknown_available'} = $same_sex_unknown_available;
	            	last;
	            }
	        	last if $subject_attributed;
	        }
		} else { # Otherwise, we attribute the death to a sex & age which is expecting for an imputation.
	        my $groups = $weekly_deaths_imputations{$death_year}->{$death_week}->{'age_groups'} // {};
	        for my $ag (sort {
	                my ($af,$at) = split /-/, $a;
	                my ($bf,$bt) = split /-/, $b;
	                ($af <=> $bf) || ($at <=> $bt)
	            } keys %$groups) {

	            for my $sx (sort keys %{ $groups->{$ag} }) {
	            	next unless $groups->{$ag}->{$sx}->{'unknown_sex_to_attrib'};
	            	my $unknown_sex_to_attrib = $groups->{$ag}->{$sx}->{'unknown_sex_to_attrib'} // die;
	            	next unless $unknown_sex_to_attrib;

	            	# Attributes the target to this cohort.
	            	$age_group = $ag;
	            	$sex = $sx;
	            	$subject_attributed = 1;
	            	$unknown_sex_to_attrib = $unknown_sex_to_attrib - 1;
            		$weekly_deaths_imputations{$death_year}->{$death_week}->{'age_groups'}->{$ag}->{$sx}->{'unknown_sex_to_attrib'} = $unknown_sex_to_attrib;
	            	last;
	            }
	        	last if $subject_attributed;
	        }
		}
		unless ($age_group) {
			$non_attributed++;
			next;
		}
		my ($from_year)        = split '-', $age_group;
		my $year_of_birth_end  = $death_year - $from_year;
		my $age_at_death       = $death_year - $year_of_birth_end;
		$quick_check{$age_group} = $year_of_birth_end;
		say $out_imput "$id,$sex,$year_of_birth_end,$week_date_of_death,$age_at_death,$death_year,$death_week,$age_group";
		$total_attributed++;
		# say "id                 : $id";
		# say "sex                : $sex";
		# say "age_group          : $age_group";
		# say "year_of_birth_end  : $year_of_birth_end";
		# say "week_date_of_death : $week_date_of_death";
		# say "age_at_death       : $age_at_death";
		# say "death_year         : $death_year";
		# say "death_week         : $death_week";

	}
	say "";
	p%quick_check;
	say "-" x 50;
	say "non_attributed     : $non_attributed";
	say "total_attributed   : $total_attributed";
	say "-" x 50;
}