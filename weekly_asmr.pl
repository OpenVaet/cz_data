#!/usr/bin/perl
use strict;
use warnings;
use 5.30.0;
no autovivification;
binmode STDOUT, ":utf8";
use utf8;
use List::Util qw(sum);
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
my %mzcr_imputed_data = ();
my %population_stats  = ();
my $mzcr_origin_file  = "data/mzcr_no_or_first_infection_with_imputation.csv";

# =============================================================================
# ESP 2013
# =============================================================================
my @esp_age_start = (0, 1, (5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85), 90);
my @esp_age_end   = (0, 4, (9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,89), 999);
my @esp_pop       = (
    1000,  # 0
    4000,  # 1-4
    5500,  # 5-9
    5500,  # 10-14
    5500,  # 15-19
    6000,  # 20-24
    6000,  # 25-29
    6500,  # 30-34
    7000,  # 35-39
    7000,  # 40-44
    7000,  # 45-49
    7000,  # 50-54
    6500,  # 55-59
    6000,  # 60-64
    5500,  # 65-69
    5000,  # 70-74
    4000,  # 75-79
    2500,  # 80-84
    1500,  # 85-89
    1000   # 90+
);

my $esp_total = sum(@esp_pop);
print "ESP 2013 total population: $esp_total\n";

# Build helper structure
my @esp;
for my $i (0..$#esp_pop) {
    push @esp, {
        idx        => $i+1,
        age_start  => $esp_age_start[$i],
        age_end    => $esp_age_end[$i],
        esp_pop    => $esp_pop[$i],
        age_group  => ($esp_age_end[$i] == 999)
            ? "$esp_age_start[$i]-999"
            : "$esp_age_start[$i]-$esp_age_end[$i]",
        age_mid    => ($esp_age_end[$i] == 999)
            ? ($esp_age_start[$i] + 2.5)
            : (($esp_age_start[$i] + $esp_age_end[$i]) / 2.0),
    };
}

# Labels & headers for per-age outputs
my @age_labels = map {
    my $s = $_->{age_start};
    my $e = $_->{age_end};
    ($e == 999) ? "90p" : "${s}_$e"   # "90p" for 90+
} @esp;

my $csv;
BEGIN {
    eval { require Text::CSV_XS; Text::CSV_XS->import(); 1 }
      ? ($csv = Text::CSV_XS->new({ binary => 1, eol => "\n" }))
      : do { require Text::CSV; Text::CSV->import(); $csv = Text::CSV->new({ binary => 1, eol => "\n" }) };
}

load_mzcr();

my %population_baseline = ();
population_at_baseline();

sub population_at_baseline {
	for my $age_group (sort keys %{$population_stats{'population'}}) {
		my $total_deaths     = $population_stats{'population'}->{$age_group}->{'total_deaths'}     // die;
		my $total_population = $population_stats{'population'}->{$age_group}->{'total_population'} // die;
		$population_baseline{$age_group}->{'total_unvaccinated'} = $total_deaths + $total_population;
        $population_baseline{$age_group}->{'total_vaccinated'}   = 0;
        $population_baseline{$age_group}->{'deaths'}->{'unvaccinated'} = 0;
        $population_baseline{$age_group}->{'deaths'}->{'vaccinated'}   = 0;
	}
}

# Collect bounds from both vaccinated and deaths
my ($min_year, $min_week, $max_year, $max_week);
for my $y (keys %{$population_stats{'vaccinated'}}) {
    for my $w (keys %{$population_stats{'vaccinated'}->{$y}}) {
        ($min_year,$min_week) = ($y+0,$w+0) if !defined $min_year || $y < $min_year || ($y==$min_year && $w < $min_week);
        ($max_year,$max_week) = ($y+0,$w+0) if !defined $max_year || $y > $max_year || ($y==$max_year && $w > $max_week);
    }
}
for my $y (keys %{$population_stats{'deaths'}}) {
    for my $w (keys %{$population_stats{'deaths'}->{$y}}) {
        ($min_year,$min_week) = ($y+0,$w+0) if !defined $min_year || $y < $min_year || ($y==$min_year && $w < $min_week);
        ($max_year,$max_week) = ($y+0,$w+0) if !defined $max_year || $y > $max_year || ($y==$max_year && $w > $max_week);
    }
}

sub next_week {
    my ($y,$w) = @_;
    my $wiy = iso_weeks_from_year($y);
    return ($w < $wiy) ? ($y, $w+1) : ($y+1, 1);
}

# p%population_baseline;

open my $out,      '>', 'outputs/weekly_death_rates.csv';
open my $out_asmr, '>', 'outputs/weekly_asmr.csv';
say $out      "year,week,age_group,esp_pop,total_unvaccinated,total_vaccinated,vaccinated_this_week," .
              "deaths_vaccinated,deaths_unvaccinated," .
              "rate_per_100000_unvaccinated,rate_per_100000_vaccinated,esp_rate_unvaccinated,esp_rate_vaccinated";
say $out_asmr "year,week,asmr_unvaccinated,asmr_vaccinated,asmr_total";
my ($year,$week) = ($min_year, $min_week);
while (defined $year && ($year < $max_year || ($year == $max_year && $week <= $max_week))) {
    my %weekly_data = ();

    for my $age_group_data (@esp) {
        my $age_group  = %$age_group_data{'age_group'} // die;
        my ($from_age) = split '-', $age_group;
        # next unless $age_group eq '60-64';
        next if $from_age < 15;
        my $esp_pop    = %$age_group_data{'esp_pop'}   // die;
        my $total_unvaccinated = $population_baseline{$age_group}->{'total_unvaccinated'} // die "age_group : $age_group";
        my $total_vaccinated   = $population_baseline{$age_group}->{'total_vaccinated'}   // die "age_group : $age_group";
        # say "-" x 50;
        # say "year                         : $year";
        # say "week                         : $week";
        # say "-" x 50;
        # say "Before :";
        # say "total_unvaccinated           : $total_unvaccinated";
        # say "total_vaccinated             : $total_vaccinated";

        # Retrieving how many people have been vaccinated on this week, and increasing / decreasing each population.
        my $vaccinated_this_week = $population_stats{'vaccinated'}->{$year}->{$week}->{$age_group} // 0;

        # Adjusting baseline.
        $total_vaccinated   += $vaccinated_this_week;
        $total_unvaccinated -= $vaccinated_this_week;

        # Retrieving vaccinated & unvaccinated deaths on week.
        my $deaths_vaccinated   = $population_stats{'deaths'}->{$year}->{$week}->{$age_group}->{'1'} // 0;
        my $deaths_unvaccinated = $population_stats{'deaths'}->{$year}->{$week}->{$age_group}->{'0'} // 0;
        $population_baseline{$age_group}->{'deaths'}->{'unvaccinated'} += $deaths_unvaccinated;
        $population_baseline{$age_group}->{'deaths'}->{'vaccinated'}   += $deaths_vaccinated;

        # say "-" x 50;
        # say "age_group                    : $age_group";
        # say "esp_pop                      : $esp_pop";
        # say "vaccinated_this_week         : $vaccinated_this_week";
        # say "deaths_vaccinated            : $deaths_vaccinated";
        # say "deaths_unvaccinated          : $deaths_unvaccinated";

        # Rates per 100.000 P/Years.
        my ($rate_per_100000_unvaccinated, $rate_per_100000_vaccinated, $esp_rate_unvaccinated, $esp_rate_vaccinated) = (0, 0, 0, 0, 0, 0);

        if ($total_unvaccinated) {
            $rate_per_100000_unvaccinated = nearest(0.0000001, $deaths_unvaccinated * 100000 * 52.18 / $total_unvaccinated);
            $esp_rate_unvaccinated        = nearest(0.0000001, $rate_per_100000_unvaccinated * $esp_pop / 100000);
        }
        if ($total_vaccinated) {
            $rate_per_100000_vaccinated   = nearest(0.0000001, $deaths_vaccinated   * 100000 * 52.18 / $total_vaccinated);
            $esp_rate_vaccinated          = nearest(0.0000001, $rate_per_100000_vaccinated   * $esp_pop / 100000);
        }

        # Combined (vaccinated + unvaccinated) rate in this age group
        my ($rate_per_100000_total, $esp_rate_total) = (0, 0);
        my $den_total = $total_unvaccinated + $total_vaccinated;
        if ($den_total) {
            my $deaths_total = $deaths_unvaccinated + $deaths_vaccinated;
            $rate_per_100000_total = nearest(0.0000001, $deaths_total * 100000 * 52.18 / $den_total);
            $esp_rate_total        = nearest(0.0000001, $rate_per_100000_total * $esp_pop / 100000);
        }

        $weekly_data{$age_group}->{'esp_rate_unvaccinated'} = $esp_rate_unvaccinated;
        $weekly_data{$age_group}->{'esp_rate_vaccinated'}   = $esp_rate_vaccinated;
        $weekly_data{$age_group}->{'esp_rate_total'}        = $esp_rate_total;
        $weekly_data{$age_group}->{'esp_pop'}               = $esp_pop;


        # Adjusts population, removing deads.
        $total_vaccinated   -= $deaths_vaccinated;
        $total_unvaccinated -= $deaths_unvaccinated;
        $population_baseline{$age_group}->{'total_unvaccinated'} = $total_unvaccinated;
        $population_baseline{$age_group}->{'total_vaccinated'}   = $total_vaccinated;

        # say "-" x 50;
        # say "After :";
        # say "total_unvaccinated           : $total_unvaccinated";
        # say "total_vaccinated             : $total_vaccinated";
        # say "rate_per_100000_unvaccinated : $rate_per_100000_unvaccinated";
        # say "rate_per_100000_vaccinated   : $rate_per_100000_vaccinated";
        # say "esp_rate_unvaccinated        : $esp_rate_unvaccinated";
        # say "esp_rate_vaccinated          : $esp_rate_vaccinated";

        say $out "$year,$week,$age_group,$esp_pop,$total_unvaccinated,$total_vaccinated,$vaccinated_this_week," .
                 "$deaths_vaccinated,$deaths_unvaccinated," .
                 "$rate_per_100000_unvaccinated,$rate_per_100000_vaccinated,$esp_rate_unvaccinated,$esp_rate_vaccinated";
    }

    # ==== weekly ASMR (per 100,000) ====
    my @ags = keys %weekly_data;

    my $esp_num_unvax = sum(map { $weekly_data{$_}->{'esp_rate_unvaccinated'} // 0 } @ags) // 0;
    my $esp_num_vax   = sum(map { $weekly_data{$_}->{'esp_rate_vaccinated'}   // 0 } @ags) // 0;
    my $esp_num_total = sum(map { $weekly_data{$_}->{'esp_rate_total'}        // 0 } @ags) // 0; # NEW

    my $esp_den_scaled = (sum(map { $weekly_data{$_}->{'esp_pop'} // 0 } @ags) // 0) / 100000;

    my ($asmr_unvaccinated, $asmr_vaccinated, $asmr_total) = (0, 0, 0);
    if ($esp_den_scaled) {
        $asmr_unvaccinated = nearest(0.0000001, $esp_num_unvax / $esp_den_scaled);
        $asmr_vaccinated   = nearest(0.0000001, $esp_num_vax   / $esp_den_scaled);
        $asmr_total        = nearest(0.0000001, $esp_num_total / $esp_den_scaled); # NEW
    }

    say $out_asmr "$year,$week,$asmr_unvaccinated,$asmr_vaccinated,$asmr_total";

    # advance week
    ($year,$week) = next_week($year,$week);
}
close $out;
close $out_asmr;

p%population_baseline;

sub load_mzcr {
	
    my $total_rows = 12125969;
    my ($cpt, $cur) = (0, 0);
    my ($csv_r, $fh, $headers, $idx) = open_csv_reader($mzcr_origin_file);
    while (my $row = $csv_r->getline_hr($fh)) {

        my $id                    = $row->{'id'}                // die;
        my $week_date_of_death    = $row->{'week_date_of_death'};
        my $age_group             = $row->{'age_group'}         // die;
        # next unless $age_group eq '60-64';
        my $year_of_birth_end     = $row->{'year_of_birth_end'} // die;
        my $sex                   = $row->{'sex'}               // die;
        my $age_at_death          = $row->{'age_at_death'};
        my $alive_on_jan_1st_2024 = 1; # By default, the individual is alive on January 1st, 2024.
        my $comp_death;                # If he died before or on January 1st, keeps track of "when"
        if ($week_date_of_death) {
        	$comp_death = $week_date_of_death;
        	$comp_death =~ s/\D//g;
        	if ($comp_death <= 20240101) {
	        	$alive_on_jan_1st_2024 = 0;
        	} else {
        		$comp_death = undef;
        	}
        }
        $cur++; $cpt++;
        if ($cpt == 1000) { $cpt = 0; STDOUT->printflush("\rParsing MZCR - [$cur / $total_rows]"); }

        # If the subject received a dose, checks that he received it.
        # If he died, consider him vaccinated on death.
        my $comp_dose;
        my $vaccinated_on_death = 0;
    	my $date_first_dose = $row->{'Date_First_Dose'};
    	if ($date_first_dose) {
    		my ($vax_year, $vax_week) = split '-', $date_first_dose;
    		if ($vax_year < 2024) {
    			$comp_dose = iso_week_monday_date($date_first_dose);
    			$comp_dose =~ s/\D//g;
    			if ($comp_death) {
    				die if ($comp_death < $comp_dose);
    				$vaccinated_on_death = 1;
    			}
	    		$vax_week = $vax_week + 0;

                # Keeps track of how many vaccinations occurred this specific week.
	    		$population_stats{'vaccinated'}->{$vax_year}->{$vax_week}->{$age_group}++;
    		}
    	}

    	# If the subject is alive on January 1st 2024, he is part of the population...
        if ($alive_on_jan_1st_2024) {
        	$population_stats{'population'}->{$age_group}->{'total_population'}++;
        } else { # Otherwise he counts as dead based on his vaccination status.
	    	($death_year, $death_week) = ymd_to_iso_year_week($week_date_of_death);
			$death_week = $death_week + 0;
	    	$population_stats{'deaths'}->{$death_year}->{$death_week}->{$age_group}->{$vaccinated_on_death}++;
	    	$population_stats{'population'}->{$age_group}->{'total_deaths'}++;
        }

        # last if $cur >= 20000;
    }
    # $csv_r->eof or die "CSV error in $mzcr_origin_file: " . $csv_r->error_diag;
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

sub iso_week_monday_date {
    my ($iso_week) = @_;
    die "Expect ISO 'YYYY-WW' like 2015-25 or 2015-W25\n"
        unless defined $iso_week && $iso_week =~ /^\s*(\d{4})[- ]?W?(\d{1,2})\s*$/;

    my ($year, $week) = ($1, $2);

    my $weeks_in_year = iso_weeks_from_year($year);
    die "Year $year has only $weeks_in_year ISO weeks\n"
        if $week < 1 || $week > $weeks_in_year;

    # ISO week 1 = week containing Jan 4; take its Monday, then add (week-1)*7 days
    my $jan4          = Time::Piece->strptime(sprintf('%04d-01-04', $year), '%Y-%m-%d');
    my $iso_dow       = $jan4->strftime('%u');  # 1..7, Monday=1
    my $week1_monday  = $jan4 - Time::Seconds::ONE_DAY * ($iso_dow - 1);
    my $monday        = $week1_monday + Time::Seconds::ONE_DAY * 7 * ($week - 1);

    return $monday->ymd;  # "YYYY-MM-DD"
}

sub iso_weeks_from_year {
    my ($year) = @_;
    # The week containing Dec 28 is always the last ISO week; its %V is 52 or 53
    my $dec28 = Time::Piece->strptime(sprintf('%04d-12-28', $year), '%Y-%m-%d');
    return 0 + $dec28->strftime('%V');
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