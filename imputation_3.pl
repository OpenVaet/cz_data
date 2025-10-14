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
say "----- Post-imputation Statistics -----";
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

# --- helpers ---
sub ymd_to_iso_week {
    my ($ymd) = @_;
    die "Bad date: $ymd" unless defined $ymd && $ymd =~ /\A\d{4}-\d{2}-\d{2}\z/;

    my $t           = Time::Piece->strptime($ymd, "%Y-%m-%d");
    my $iso_week    = $t->strftime("%V");         # ISO week number 01..53
    my $iso_wday    = $t->strftime("%u");         # ISO weekday 1=Mon..7=Sun
    my $week_monday = ($t - ($iso_wday - 1) * ONE_DAY)->strftime("%Y-%m-%d");
    return (
        $iso_week
    );
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
		my $week = $row->{'TIME_PERIOD: Time'} // die;
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
		my ($year, $week_number) = split '-', $week;
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
		$week_number =~ s/W//;
		die unless looks_like_number($week_number);
		$eurostat_deaths{$year}->{$week_number}->{$age_group}->{$sex_code} += $value;
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
		my $week_date_of_death    = $row->{'week_date_of_death'} // die;
		my $year_of_birth_end     = $row->{'year_of_birth_end'}  // die;
		my $age_at_death          = $row->{'age_at_death'}       // die;
		my $sex                   = $row->{'sex'}                // die;
		my $id                    = $row->{'id'}                 // die;
		my ($death_year)          = split '-', $week_date_of_death;
		my $death_week            = ymd_to_iso_week($week_date_of_death);
		my ($from_year, $to_year) = from_year_to_year_from_age($age_at_death);
		my $age_group             = "$from_year-$to_year";
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
			if ($year eq 2020) {
				next if $week < 10;
			} elsif ($year eq 2024) {
				next;
			}
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
			my ($death_year) = split '-', $week_date_of_death;
			my $death_week   = ymd_to_iso_week($week_date_of_death);
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

for my $year (sort{$a <=> $b} keys %no_yob_deaths) {
	for my $week (sort{$a <=> $b} keys %{$no_yob_deaths{$year}}) {
		next unless exists $weeks_requiring_imputation{$year}->{$week} && exists $no_yob_deaths{$year}->{$week};

		# Fetching how many unknown deaths we can impute on this specific week.
		my $weekly_unknown_sex_unknown_available = $no_yob_deaths{$year}->{$week}->{'U'}->{'total_deaths_with_no_dob'} // 0;

		# Fetching attribution data : how many deaths known (for control), how many are missing.
		my ($weekly_total_eurostat_deaths, $weekly_total_known_yob_mzcr_deaths, $weekly_total_to_attribute, $weekly_total_same_sex_unknown_available) = (0, 0, 0, 0);
		for my $age_group (sort keys %{$weeks_requiring_imputation{$year}->{$week}}) {
			my ($from_year, $to_year) = split '-', $age_group;
			for my $sex (sort keys %{$weeks_requiring_imputation{$year}->{$week}->{$age_group}}) {
				$weekly_total_eurostat_deaths += $weeks_requiring_imputation{$year}->{$week}->{$age_group}->{$sex}->{'eurostat_deaths'};
				$weekly_total_known_yob_mzcr_deaths += $weeks_requiring_imputation{$year}->{$week}->{$age_group}->{$sex}->{'mzcr_deaths'};
				$weekly_total_to_attribute += $weeks_requiring_imputation{$year}->{$week}->{$age_group}->{$sex}->{'missing_in_mzcr'};
				my $same_sex_unknown_available = $no_yob_deaths{$year}->{$week}->{$sex}->{'total_deaths_with_no_dob'} // 0;
				$weekly_total_same_sex_unknown_available += $same_sex_unknown_available;
			}
		}

		# % of weekly deaths "available" & % of share in each group.
		my $overall_available    = $weekly_total_same_sex_unknown_available + $weekly_unknown_sex_unknown_available;
		my $weekly_available_pct = nearest(0.001, $overall_available * 100 / $weekly_total_to_attribute);
		say "-" x 50;
		say "year                                    : $year";
		say "week                                    : $week";
		say "weekly_total_eurostat_deaths            : $weekly_total_eurostat_deaths";
		say "weekly_total_known_yob_mzcr_deaths      : $weekly_total_known_yob_mzcr_deaths";
		say "weekly_total_to_attribute               : $weekly_total_to_attribute";
		say "weekly_unknown_sex_unknown_available    : $weekly_unknown_sex_unknown_available";
		say "weekly_total_same_sex_unknown_available : $weekly_total_same_sex_unknown_available";
		say "weekly_available_pct                    : $weekly_available_pct %";
		# $weekly_deaths_imputations{$year}->{$week}->{'weekly_total_to_attribute'}   = $weekly_total_to_attribute;
		# $weekly_deaths_imputations{$year}->{$week}->{'total_available'}      = $total_available;
		# $weekly_deaths_imputations{$year}->{$week}->{'weekly_available_pct'} = $weekly_available_pct;

		# Total deaths to be attributed in each group : either on known sex (default) or imputed sex.
		for my $age_group (sort keys %{$weeks_requiring_imputation{$year}->{$week}}) {
			my ($from_year, $to_year) = split '-', $age_group;
			for my $sex (sort keys %{$weeks_requiring_imputation{$year}->{$week}->{$age_group}}) {
				my $missing_in_mzcr                 = $weeks_requiring_imputation{$year}->{$week}->{$age_group}->{$sex}->{'missing_in_mzcr'} // next;
				my $eurostat_deaths                 = $weeks_requiring_imputation{$year}->{$week}->{$age_group}->{$sex}->{'eurostat_deaths'} // die;
				my $same_sex_unknown_available      = $no_yob_deaths{$year}->{$week}->{$sex}->{'total_deaths_with_no_dob'} // 0;
				my $age_group_share_of_total_deaths = nearest(0.001, $eurostat_deaths * 100 / $weekly_total_eurostat_deaths);
				my $deaths_to_attrib_on_ag_sex      = nearest(1, ($overall_available * $age_group_share_of_total_deaths / 100) * $weekly_available_pct / 100);
				say "-" x 50;
				say "age_group                               : $age_group";
				say "sex                                     : $sex";
				say "missing_in_mzcr                         : $missing_in_mzcr";
				say "eurostat_deaths                         : $eurostat_deaths";
				say "same_sex_unknown_available              : $same_sex_unknown_available %";
				say "age_group_share_of_total_deaths         : $age_group_share_of_total_deaths %";
				say "deaths_to_attrib_on_ag_sex              : $deaths_to_attrib_on_ag_sex";
			}
		}
		die;
		# for my $from_year (sort{$a <=> $b} keys %ages_to_attrib) {
		# 	my $age_group = $ages_to_attrib{$from_year}->{'age_group'} // die;
		# 	for my $sex (sort keys %{$ages_to_attrib{$from_year}->{'sexes'}}) {
		# 		my $eurostat_deaths = $ages_to_attrib{$from_year}->{'sexes'}->{$sex}->{'eurostat_deaths'} // die;
		# 		my $share_of_total_deaths = nearest(0.001, $eurostat_deaths * 100 / $weekly_total_to_attribute);
		# 		my $deaths_to_attrib = nearest(1, ($eurostat_deaths * $weekly_available_pct / 100 * $share_of_total_deaths / 100));
		# 		$weekly_deaths_imputations{$year}->{$week}->{'age_groups'}->{$age_group}->{$sex}->{'eurostat_deaths'}       = $eurostat_deaths;
		# 		$weekly_deaths_imputations{$year}->{$week}->{'age_groups'}->{$age_group}->{$sex}->{'deaths_to_attrib'}      = $deaths_to_attrib;
		# 		$weekly_deaths_imputations{$year}->{$week}->{'age_groups'}->{$age_group}->{$sex}->{'share_of_total_deaths'} = $share_of_total_deaths;
		# 	}
		# }
		# # p$no_yob_deaths{$year}->{$week};
		# # die;
	}
}
say "-" x 50;

__END__

# p%no_yob_deaths;

# Ensure output dir exists
my $out_path = 'outputs/layer_3_deaths_by_ages_and_sexes.csv';
make_path('outputs') unless -d 'outputs';

# Prefer Text::CSV_XS, fall back to Text::CSV
my $csv;
BEGIN {
    eval { require Text::CSV_XS; Text::CSV_XS->import(); 1 }
      ? ($csv = Text::CSV_XS->new({ binary => 1, eol => "\n" }))
      : do { require Text::CSV; Text::CSV->import(); $csv = Text::CSV->new({ binary => 1, eol => "\n" }) };
}

open my $fh, '>:encoding(UTF-8)', $out_path or die "Can't write $out_path: $!";

# Header
$csv->print($fh, [
    'year',
    'week',
    'age_group',
    'sex',
    'eurostat_deaths',
    'deaths_to_attrib',
    'share_of_total_deaths',
    'weekly_total_to_attribute',
    'total_available',
    'weekly_available_pct',
]);

for my $year (sort { $a <=> $b } keys %weekly_deaths_imputations) {
    for my $week (sort { $a <=> $b } keys %{ $weekly_deaths_imputations{$year} }) {

        my $weekly_total_to_attribute   = $weekly_deaths_imputations{$year}->{$week}->{'weekly_total_to_attribute'}   // 0;
        my $total_available      = $weekly_deaths_imputations{$year}->{$week}->{'total_available'}      // 0;
        my $weekly_available_pct = $weekly_deaths_imputations{$year}->{$week}->{'weekly_available_pct'} // 0;

        my $groups = $weekly_deaths_imputations{$year}->{$week}->{'age_groups'} // {};
        for my $age_group (sort {
                my ($af,$at) = split /-/, $a;
                my ($bf,$bt) = split /-/, $b;
                ($af <=> $bf) || ($at <=> $bt)
            } keys %$groups) {

            for my $sex (sort keys %{ $groups->{$age_group} }) {
                my $eurostat_deaths       = $groups->{$age_group}->{$sex}->{'eurostat_deaths'}       // 0;
                my $deaths_to_attrib      = $groups->{$age_group}->{$sex}->{'deaths_to_attrib'}      // 0;
                my $share_of_total_deaths = $groups->{$age_group}->{$sex}->{'share_of_total_deaths'} // 0;

                $csv->print($fh, [
                    $year,
                    $week,
                    $age_group,
                    $sex,
                    $eurostat_deaths,
                    $deaths_to_attrib,
                    $share_of_total_deaths,
                    $weekly_total_to_attribute,
                    $total_available,
                    $weekly_available_pct,
                ]);
            }
        }
    }
}

close $fh or warn "Couldn't close $out_path: $!";

say "Wrote $out_path";

# --- Outputting the imputed deaths ---
open my $out_imput, '>', 'outputs/imputation_layer_3.csv';
say $out_imput "id,sex,year_of_birth_end,week_date_of_death,age_at_death";
my $total_ids = keys %no_yob_ids;
my $current = 0;
for my $id (sort keys %no_yob_ids) {
	my $sex        = $no_yob_ids{$id}->{'sex'}        // die;
	my $death_year = $no_yob_ids{$id}->{'death_year'} // die;
	my $death_week = $no_yob_ids{$id}->{'death_week'} // die;
	my $from_year  = $no_yob_ids{$id}->{'from_year'}  // die;
	my $to_year    = $no_yob_ids{$id}->{'to_year'}    // die;
	STDOUT->printflush("\rImputing unknown DOB/Gender who died - MZCR - [$current / $out_imput]");

}

close $out_imput;
say "";
