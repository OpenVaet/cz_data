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

my $eurostat_deaths_file  = "data/demo_r_mwk_05_linear_2_0.csv";
my $mzcr_file             = "data/mzcr_no_or_first_infection.csv";
my $offsets_file          = 'outputs/non_imputed_deaths_offset.csv';
my $no_imput_deaths_file  = "outputs/deaths_non_imputed.csv";
my $imputed_deaths_file   = "outputs/deaths_imputed.csv";

my %eurostat_deaths       = ();
my %mzcr_deaths           = ();
my %mzcr_unknown          = ();
my %weekly_deaths_offsets = ();

# --- Eurostat deaths file ---
load_eurostats();

# --- MZCR file ---
load_mzcr();

# --- Verifies if weeks in mzcr_deaths are also always present in Eurostats.
for my $year (sort{$a <=> $b} keys %mzcr_deaths) {
	for my $week (sort{$a <=> $b} keys %{$mzcr_deaths{$year}}) {
		die unless exists $eurostat_deaths{$year}->{$week};
	}
}

# --- Offset in deaths when sex & YOB is known ---
my @age_groups = ("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-999");
my ($total_deaths_extra, $total_deaths_missing) = (0, 0);
open my $out_deaths, '>', $offsets_file or die $!;
say $out_deaths "year,week,age_group,unknown_week_deaths,mzcr_deaths,eurostat_deaths,eurostat_minus_mzcr";
for my $year (sort{$a <=> $b} keys %mzcr_deaths) {
	for my $week (sort{$a <=> $b} keys %{$mzcr_deaths{$year}}) {
		if ($year eq 2020) {
			next if $week < 10;
		} elsif ($year eq 2024) {
			next;
		}
		my $unknown_week_deaths = $mzcr_unknown{$year}->{$week} // 0;
		for my $age_group (@age_groups) {
			my $eurostat_deaths      = $eurostat_deaths{$year}->{$week}->{$age_group} // 0;
			my $mzcr_deaths          = $mzcr_deaths{$year}->{$week}->{$age_group}     // 0;
			my $eurostat_minus_mzcr  = $eurostat_deaths - $mzcr_deaths;
			if ($eurostat_minus_mzcr > 0) {
				$total_deaths_extra += $eurostat_minus_mzcr;
			} else {
				$total_deaths_missing += $eurostat_minus_mzcr;
			}
			$weekly_deaths_offsets{$year}->{$week}->{$age_group}->{'mzcr_deaths'}         = $mzcr_deaths;
			$weekly_deaths_offsets{$year}->{$week}->{$age_group}->{'eurostat_deaths'}     = $eurostat_deaths;
			$weekly_deaths_offsets{$year}->{$week}->{$age_group}->{'eurostat_minus_mzcr'} = $eurostat_minus_mzcr;
			say $out_deaths "$year,$week,$age_group,$unknown_week_deaths,$mzcr_deaths,$eurostat_deaths,$eurostat_minus_mzcr";
		}
	}
}
close $out_deaths;

# --- Rendering layer 1 statistics ---
$total_deaths_extra   = abs($total_deaths_extra);
$total_deaths_missing = abs($total_deaths_missing);
say "-" x 50;
say "----- Pre-imputation Recap. -----";
say "-" x 50;
say "total_deaths_extra       : $total_deaths_extra";
say "total_deaths_missing     : $total_deaths_missing";
say "-" x 50;

# --- Applying layering patterns
for my $year (sort{$a <=> $b} keys %weekly_deaths_offsets) {
	for my $week (sort{$a <=> $b} keys %{$weekly_deaths_offsets{$year}}) {
		if ($year == 2020 || ($year == 2021 && $week <= 10)) {

		} else {

		}
	}
}

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

sub from_year_to_year_from_age_str {
	my $age = shift;
	my ($from_year, $to_year);
	if ($age eq "Y_LT5: Less than 5 years") {
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
	return ($from_year, $to_year);
}

# --- functions ---
sub load_mzcr {
	my $mzcr_total_rows = 12125969;
	my ($cpt, $mzcr_current_rows) = (0, 0);
	my ($mzcr_csv, $mzcr_fh, $mzcr_headers, $mzcr_idx) = open_csv_reader($mzcr_file);
	open my $out, '>', $no_imput_deaths_file or die $!;
	say $out "id,sex,year_of_birth_end,week_date_of_death,death_year,death_week";
	while (my $row = $mzcr_csv->getline_hr($mzcr_fh)) {
		$mzcr_current_rows++;
		$cpt++;
		if ($cpt == 10000) {
			$cpt = 0;
			STDOUT->printflush("\rParsing MZCR - [$mzcr_current_rows / $mzcr_total_rows]");
		}
		my $week_date_of_death = $row->{'week_date_of_death'} // die;
		my $year_of_birth_end  = $row->{'year_of_birth_end'}  // die;
		my $age_at_death       = $row->{'age_at_death'}       // die;
		my $gender             = $row->{'gender'}             // die;
		my $id                 = $row->{'ID'}                 // die;
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
			die "$week_date_of_death -> $death_year, $death_week" unless $death_year;
			say $out "$id,$sex,$year_of_birth_end,$week_date_of_death,$death_year,$death_week";
			if ($year_of_birth_end) {
				my ($from_year, $to_year) = from_year_to_year_from_age($age_at_death);
				my $age_group             = "$from_year-$to_year";
				$mzcr_deaths{$death_year}->{$death_week}->{$age_group}++;
			} else {
				$mzcr_unknown{$death_year}->{$death_week}++;
			}
		}
	}
	close $out;
	$mzcr_csv->eof or die "CSV error in $mzcr_file: " . $mzcr_csv->error_diag;
	close $mzcr_fh or warn "Couldn't close $mzcr_file: $!";
	STDOUT->printflush("\rParsing MZCR - [$mzcr_current_rows / $mzcr_total_rows]");
	say "";
}

sub load_eurostats {
	STDOUT->printflush("\rParsing Eurostat Deaths");
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
		my ($year, $week_number)  = split '-', $week;
		next if $age eq "UNK: Unknown";
		my ($from_year, $to_year) = from_year_to_year_from_age_str($age);
		my $age_group             = "$from_year-$to_year";
		$week_number              =~ s/W//;
		die unless looks_like_number($week_number);
		$year                     = 0 + $year;
		$week_number              = 0 + $week_number;
		$eurostat_deaths{$year}->{$week_number}->{$age_group} += $value;
	}
	$euro_csv->eof or die "CSV error in $eurostat_deaths_file: " . $euro_csv->error_diag;
	close $euro_fh or warn "Couldn't close $eurostat_deaths_file: $!";
	say "";
}