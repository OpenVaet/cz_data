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

# --------------------------------------------------
# INPUTS
# --------------------------------------------------
my %mzcr_imputed_data     = ();
my $mzcr_origin_file      = "data/mzcr_no_or_first_infection.csv";
my $mzcr_deaths_file      = 'outputs/imputation_layer_3.csv';
my $mzcr_imputed_pop_file = "outputs/imputed_population_yob_2024.csv";

my ($mzcr_total_pop,
    $eurostat_total_pop,
    $yobs_missing) = (0, 0, 0);

my $csv;
BEGIN {
    eval { require Text::CSV_XS; Text::CSV_XS->import(); 1 }
      ? ($csv = Text::CSV_XS->new({ binary => 1, eol => "\n" }))
      : do { require Text::CSV; Text::CSV->import(); $csv = Text::CSV->new({ binary => 1, eol => "\n" }) };
}

# --- Age groups & sexes used everywhere ---
my @age_groups = ("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-999");
my @sexes = ('M', 'F');

load_mzcr_imputed_deaths();

load_mzcr_imputed_population();

load_mzcr_origin();

sub load_mzcr_imputed_deaths {

    my $total_rows = 556903;
    my ($cpt, $cur) = (0, 0);
    my ($csv_r, $fh, $headers, $idx) = open_csv_reader($mzcr_deaths_file);

    while (my $row = $csv_r->getline_hr($fh)) {
        $cur++; $cpt++;
        if ($cpt == 1000) { $cpt = 0; STDOUT->printflush("\rParsing MZCR Imputed Deaths - [$cur / $total_rows]"); }
        my $id                 = $row->{'id'}                 // die;
        my $week_date_of_death = $row->{'week_date_of_death'} // die;
        my $year_of_birth_end  = $row->{'year_of_birth_end'}  // die;
        my $sex                = $row->{'sex'}                // die;
        my $age_at_death       = $row->{'age_at_death'}       // die;
        my $death_year         = $row->{'death_year'}         // die;
        my $death_week         = $row->{'death_week'}         // die;
        my $age_group          = $row->{'age_group'}          // die;
        $mzcr_imputed_data{$id}->{'sex'}                = $sex;
        $mzcr_imputed_data{$id}->{'week_date_of_death'} = $week_date_of_death;
        $mzcr_imputed_data{$id}->{'year_of_birth_end'}  = $year_of_birth_end;
        $mzcr_imputed_data{$id}->{'age_at_death'}       = $age_at_death;
        $mzcr_imputed_data{$id}->{'death_year'}         = $death_year;
        $mzcr_imputed_data{$id}->{'death_week'}         = $death_week;
        $mzcr_imputed_data{$id}->{'age_group'}          = $age_group;
    }
    $csv_r->eof or die "CSV error in $mzcr_deaths_file: " . $csv_r->error_diag;
    close $fh or warn "Couldn't close $mzcr_deaths_file: $!";
    STDOUT->printflush("\rParsing MZCR Imputed Deaths - [$cur / $total_rows]"); say "";
}

sub load_mzcr_imputed_population {
	
    my $total_rows = 1567297;
    my ($cpt, $cur) = (0, 0);
    my ($csv_r, $fh, $headers, $idx) = open_csv_reader($mzcr_imputed_pop_file);

    while (my $row = $csv_r->getline_hr($fh)) {
        $cur++; $cpt++;
        if ($cpt == 1000) { $cpt = 0; STDOUT->printflush("\rParsing MZCR Imputed Population - [$cur / $total_rows]"); }

        my $id                 = $row->{'id'}                 // die;
        die if exists $mzcr_imputed_data{$id};
        my $year_of_birth_end  = $row->{'year_of_birth_end'}  // die;
        my $sex                = $row->{'sex'}                // die;
        my $age_group          = $row->{'age_group'}          // die;
        $mzcr_imputed_data{$id}->{'sex'}                = $sex;
        $mzcr_imputed_data{$id}->{'year_of_birth_end'}  = $year_of_birth_end;
        $mzcr_imputed_data{$id}->{'age_group'}          = $age_group;
    }
    $csv_r->eof or die "CSV error in $mzcr_imputed_pop_file: " . $csv_r->error_diag;
    close $fh or warn "Couldn't close $mzcr_imputed_pop_file: $!";
    STDOUT->printflush("\rParsing MZCR Imputed Population - [$cur / $total_rows]"); say "";
}

sub load_mzcr_origin {
	
    my $total_rows = 12125969;
    my ($cpt, $cur) = (0, 0);
    my ($csv_r, $fh, $headers, $idx) = open_csv_reader($mzcr_origin_file);
    open my $out, '>', 'data/mzcr_no_or_first_infection_with_imputation.csv';
	say $out "id,sex,year_of_birth_end,age,age_group,age_at_death,week_date_of_death,Date_First_Dose,Date_Second_Dose,Date_Third_Dose,VaccinationProductCode_First_Dose,VaccinationProductCode_Second_Dose,VaccinationProductCode_Third_Dose,week_date_of_positivity";
    while (my $row = $csv_r->getline_hr($fh)) {
        $cur++; $cpt++;
        if ($cpt == 1000) { $cpt = 0; STDOUT->printflush("\rParsing MZCR - [$cur / $total_rows]"); }

        my $id                 = $row->{'ID'}                 // die;
        my $week_date_of_death = $row->{'week_date_of_death'} // '';
        my $year_of_birth_end  = $row->{'year_of_birth_end'};
        my $gender             = $row->{'gender'};  # 1,2 or empty
        my $sex = ($gender && $gender == 1) ? 'M' : ($gender && $gender == 2) ? 'F' : 'U';

        # Retrieving imputed data.
        if (exists $mzcr_imputed_data{$id}) {
        	$sex                = $mzcr_imputed_data{$id}->{'sex'}                // die;
        	$year_of_birth_end  = $mzcr_imputed_data{$id}->{'year_of_birth_end'}  // die;
        	$week_date_of_death = $mzcr_imputed_data{$id}->{'week_date_of_death'} // '';
        }

        die if $sex eq 'U' || !$year_of_birth_end;
        my $Date_First_Dose                    = $row->{'Date_First_Dose'}                    // '';
        my $Date_Second_Dose                   = $row->{'Date_Second_Dose'}                   // '';
        my $Date_Third_Dose                    = $row->{'Date_Third_Dose'}                    // '';
        my $VaccinationProductCode_First_Dose  = $row->{'VaccinationProductCode_First_Dose'}  // '';
        my $VaccinationProductCode_Second_Dose = $row->{'VaccinationProductCode_Second_Dose'} // '';
        my $VaccinationProductCode_Third_Dose  = $row->{'VaccinationProductCode_Third_Dose'}  // '';
        my $week_date_of_positivity            = $row->{'week_date_of_positivity'}            // '';

        # Calculating age.
        my $age;
        my ($death_year, $death_week, $age_at_death) = ('', '', '');
		if ($week_date_of_death) {
			($death_year, $death_week) = ymd_to_iso_year_week($week_date_of_death);
			$age = $death_year - $year_of_birth_end;
			$age_at_death = $age;
		} else {
			$age = 2024 - $year_of_birth_end - 1;
		}
		my ($from_year, $to_year) = from_year_to_year_from_age($age);
		my $age_group = "$from_year-$to_year";

		say $out "$id,$sex,$year_of_birth_end,$age,$age_group,$age_at_death,$week_date_of_death,$Date_First_Dose,$Date_Second_Dose,$Date_Third_Dose,$VaccinationProductCode_First_Dose,$VaccinationProductCode_Second_Dose,$VaccinationProductCode_Third_Dose,$week_date_of_positivity";

    }
    close $out;
    $csv_r->eof or die "CSV error in $mzcr_origin_file: " . $csv_r->error_diag;
    close $fh or warn "Couldn't close $mzcr_origin_file: $!";
    STDOUT->printflush("\rParsing MZCR - [$cur / $total_rows]"); say "";
}

# --------------------------------------------------
# ------------------ SUBROUTINES -------------------
# --------------------------------------------------
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
        binary              => 1,
        auto_diag           => 1,
        sep_char            => $sep_char,
        quote_char          => '"',
        escape_char         => '"',
        allow_loose_quotes  => 1,
        allow_loose_escapes => 1,
        blank_is_undef      => 0,
    }) or die "Text::CSV_XS->new failed: " . Text::CSV_XS->error_diag;

    $csv->parse($header_line) or die "Header parse failed for $path: " . $csv->error_diag;
    my @headers = map { my $h = $_ // ''; $h =~ s/^\s+|\s+$//g; $h; } $csv->fields;
    $csv->column_names(\@headers);

    my %col_idx; for (my $i = 0; $i < @headers; $i++) { $col_idx{$headers[$i]} = $i; }
    return ($csv, $fh, \@headers, \%col_idx);
}

sub from_year_to_year_from_age {
    my $age = shift;
    my ($from_year, $to_year);
    if ($age <= 4)       { ($from_year, $to_year) = (0, 4) }
    elsif ($age <= 9)    { ($from_year, $to_year) = (5, 9) }
    elsif ($age <= 14)   { ($from_year, $to_year) = (10, 14) }
    elsif ($age <= 19)   { ($from_year, $to_year) = (15, 19) }
    elsif ($age <= 24)   { ($from_year, $to_year) = (20, 24) }
    elsif ($age <= 29)   { ($from_year, $to_year) = (25, 29) }
    elsif ($age <= 34)   { ($from_year, $to_year) = (30, 34) }
    elsif ($age <= 39)   { ($from_year, $to_year) = (35, 39) }
    elsif ($age <= 44)   { ($from_year, $to_year) = (40, 44) }
    elsif ($age <= 49)   { ($from_year, $to_year) = (45, 49) }
    elsif ($age <= 54)   { ($from_year, $to_year) = (50, 54) }
    elsif ($age <= 59)   { ($from_year, $to_year) = (55, 59) }
    elsif ($age <= 64)   { ($from_year, $to_year) = (60, 64) }
    elsif ($age <= 69)   { ($from_year, $to_year) = (65, 69) }
    elsif ($age <= 74)   { ($from_year, $to_year) = (70, 74) }
    elsif ($age <= 79)   { ($from_year, $to_year) = (75, 79) }
    elsif ($age <= 84)   { ($from_year, $to_year) = (80, 84) }
    elsif ($age <= 89)   { ($from_year, $to_year) = (85, 89) }
    else                 { die unless $age >= 90; ($from_year, $to_year) = (90, 999) }
    return ($from_year, $to_year);
}

