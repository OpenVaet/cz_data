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

##################################################################################
# First, re-attributing offsets which can be compensated - by balancing the      #
# deaths when too many people died in a MZCR age group to the MZCR age group     #
# immediately before or after.                                                   #
##################################################################################

my $mzcr_file               = "outputs/imputation_layer_1.csv";
my $deaths_offset_file      = 'outputs/deaths_offset.csv';
my %subjects_pool           = ();
my @sexes                   = ('M', 'F');

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

# --- Deaths offset file ---
my %deaths = ();
my ($total_deaths_extra, $total_deaths_missing) = (0, 0);
my ($deaths_csv, $deaths_fh, $deaths_headers, $deaths_idx) = open_csv_reader($deaths_offset_file);
while (my $row = $deaths_csv->getline_hr($deaths_fh)) {
	my $year                  = $row->{'year'}                  // die;
	my $week                  = $row->{'week'}                  // die;
	my $age_group             = $row->{'age_group'}             // die;
	my $eurostat_deaths       = $row->{'eurostat_deaths'}       // die;
	my $mzcr_deaths           = $row->{'mzcr_deaths'}           // die;
	my $mzcr_minus_eurostats  = $row->{'mzcr_minus_eurostats'}  // die;
	my $sex                   = $row->{'sex'}                   // die;
	if ($mzcr_minus_eurostats > 0) {
		$total_deaths_extra += $mzcr_minus_eurostats;
	} else {
		$total_deaths_missing += $mzcr_minus_eurostats;
	}
	$deaths{$year}->{$week}->{$age_group}->{$sex}->{'mzcr_minus_eurostats'} = $mzcr_minus_eurostats;
	$deaths{$year}->{$week}->{$age_group}->{$sex}->{'eurostat_deaths'}     = $eurostat_deaths;
	$deaths{$year}->{$week}->{$age_group}->{$sex}->{'mzcr_deaths'}         = $mzcr_deaths;
}
$deaths_csv->eof or die "CSV error in $deaths_offset_file: " . $deaths_csv->error_diag;
close $deaths_fh or warn "Couldn't close $deaths_offset_file: $!";

# 0-4
$total_deaths_extra = abs($total_deaths_extra);
say "[$total_deaths_extra] too many deaths in MZCR in a given sex & age group, aren't in Eurostat - and must be re-attributed to the nearest age group ($total_deaths_missing deaths from Eurostat aren't in MZCR as well).";
my %deaths_to_reattrib = ();
for my $year (sort{$a <=> $b} keys %deaths) {
	for my $week (sort{$a <=> $b} keys %{$deaths{$year}}) {
		if ($year eq 2020) {
			next if $week < 10;
		} elsif ($year eq 2024) {
			next;
		}
		for my $sex (@sexes) {
			my $eurostat_deaths       = $deaths{$year}->{$week}->{'0-4'}->{$sex}->{'eurostat_deaths'}      // 0;
			my $mzcr_deaths           = $deaths{$year}->{$week}->{'0-4'}->{$sex}->{'mzcr_deaths'}          // 0;
			my $mzcr_minus_eurostats  = $deaths{$year}->{$week}->{'0-4'}->{$sex}->{'mzcr_minus_eurostats'} // 0;
			if ($mzcr_minus_eurostats > 0) {
				my $next_val_mzcr     = $deaths{$year}->{$week}->{'5-9'}->{$sex}->{'mzcr_deaths'}          // 0;
				my $next_val_eurostat = $deaths{$year}->{$week}->{'5-9'}->{$sex}->{'eurostat_deaths'}      // 0;
				my $next_val_offset   = $deaths{$year}->{$week}->{'5-9'}->{$sex}->{'mzcr_minus_eurostats'} // 0;
				if ($next_val_offset < 0) {
					my $abs_offset   = abs($next_val_offset);
					my $to_reattrib  = 0;
					if ($abs_offset >= $mzcr_minus_eurostats) {
						$to_reattrib = $mzcr_minus_eurostats;
						say "[$year] - [$week] - The offset [$mzcr_minus_eurostats] on [0-4] can be entirely compensated ($to_reattrib). [5-9] : $next_val_mzcr MZCR vs $next_val_eurostat Eurostat";
					} else {
						$to_reattrib = $abs_offset;
						say "[$year] - [$week] - The offset [$mzcr_minus_eurostats] on [0-4] can be partially compensated ($to_reattrib). [5-9] : $next_val_mzcr MZCR vs $next_val_eurostat Eurostat";
					}
					$deaths_to_reattrib{$sex}->{$year}->{$week}->{'0-4'}->{'5-9'}  = $to_reattrib;

					# Subtract the deaths we just reattributed in the 0-4 group & recalcs offset.
					$deaths{$year}->{$week}->{'0-4'}->{$sex}->{'mzcr_deaths'}         -= $to_reattrib;
					$deaths{$year}->{$week}->{'0-4'}->{$sex}->{'mzcr_minus_eurostats'} = $deaths{$year}->{$week}->{'0-4'}->{$sex}->{'mzcr_deaths'} - $deaths{$year}->{$week}->{'0-4'}->{$sex}->{'eurostat_deaths'};

					# Adds the deaths to the 0-5 age group & recalcs offset.
					$deaths{$year}->{$week}->{'5-9'}->{$sex}->{'mzcr_deaths'}         += $to_reattrib;
					$deaths{$year}->{$week}->{'5-9'}->{$sex}->{'mzcr_minus_eurostats'} = $deaths{$year}->{$week}->{'5-9'}->{$sex}->{'mzcr_deaths'} - $deaths{$year}->{$week}->{'5-9'}->{$sex}->{'eurostat_deaths'};
				} else {
					# say "[$year] - [$week] - The offset [$mzcr_minus_eurostats] can't be compensated at all ($next_val_offset).";
				}
			}
		}
	}
}

# 5-9 to 85-89
my @age_groups = ("5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89");
for my $age_group (@age_groups) {
	my ($from_year, $to_year) = split '-', $age_group;
	my $next_from = $from_year + 5;
	my $next_to   = $to_year   + 5;
	my $next_age_grp = "$next_from-$next_to";
	my $prev_from = $from_year - 5;
	my $prev_to   = $to_year   - 5;
	my $prev_age_grp = "$prev_from-$prev_to";
	for my $year (sort{$a <=> $b} keys %deaths) {
		for my $week (sort{$a <=> $b} keys %{$deaths{$year}}) {
			if ($year eq 2020) {
				next if $week < 10;
			} elsif ($year eq 2024) {
				next;
			}
			for my $sex (@sexes) {
				my $eurostat_deaths      = $deaths{$year}->{$week}->{$age_group}->{$sex}->{'eurostat_deaths'}      // 0;
				my $mzcr_deaths          = $deaths{$year}->{$week}->{$age_group}->{$sex}->{'mzcr_deaths'}          // 0;
				my $mzcr_minus_eurostats = $deaths{$year}->{$week}->{$age_group}->{$sex}->{'mzcr_minus_eurostats'} // 0;
				if ($mzcr_minus_eurostats > 0) {
					my $next_val_mzcr     = $deaths{$year}->{$week}->{$next_age_grp}->{$sex}->{'mzcr_deaths'}          // 0;
					my $next_val_eurostat = $deaths{$year}->{$week}->{$next_age_grp}->{$sex}->{'eurostat_deaths'}      // 0;
					my $next_val_offset   = $deaths{$year}->{$week}->{$next_age_grp}->{$sex}->{'mzcr_minus_eurostats'} // 0;
					my $to_reattrib  = 0;
					if ($next_val_offset < 0) {
						my $abs_offset   = abs($next_val_offset);
						if ($abs_offset >= $mzcr_minus_eurostats) {
							$to_reattrib = $mzcr_minus_eurostats;
							say "[$year] - [$week] - The offset [$mzcr_minus_eurostats] on [$age_group] can be entirely compensated ($to_reattrib). [$next_age_grp] : $next_val_mzcr MZCR vs $next_val_eurostat Eurostat";
						} else {
							$to_reattrib = $abs_offset;
							say "[$year] - [$week] - The offset [$mzcr_minus_eurostats] on [$age_group] can be partially compensated ($to_reattrib). [$next_age_grp] : $next_val_mzcr MZCR vs $next_val_eurostat Eurostat";
						}
						$deaths_to_reattrib{$sex}->{$year}->{$week}->{$age_group}->{$next_age_grp}  = $to_reattrib;

						# Subtract the deaths we just reattributed in the $age_group group & recalcs offset.
						$deaths{$year}->{$week}->{$age_group}->{$sex}->{'mzcr_deaths'}         -= $to_reattrib;
						$deaths{$year}->{$week}->{$age_group}->{$sex}->{'mzcr_minus_eurostats'} = $deaths{$year}->{$week}->{$age_group}->{$sex}->{'mzcr_deaths'} - $deaths{$year}->{$week}->{$age_group}->{$sex}->{'eurostat_deaths'};

						# Adds the deaths to the 0-5 age group & recalcs offset.
						$deaths{$year}->{$week}->{$next_age_grp}->{$sex}->{'mzcr_deaths'}         += $to_reattrib;
						$deaths{$year}->{$week}->{$next_age_grp}->{$sex}->{'mzcr_minus_eurostats'} = $deaths{$year}->{$week}->{$next_age_grp}->{$sex}->{'mzcr_deaths'} - $deaths{$year}->{$week}->{$next_age_grp}->{$sex}->{'eurostat_deaths'};
					}
					if ($to_reattrib  < $mzcr_minus_eurostats) {
						my $still_missing     = $mzcr_minus_eurostats - $to_reattrib;
						my $prev_val_mzcr     = $deaths{$year}->{$week}->{$prev_age_grp}->{$sex}->{'mzcr_deaths'}         // 0;
						my $prev_val_eurostat = $deaths{$year}->{$week}->{$prev_age_grp}->{$sex}->{'eurostat_deaths'}     // 0;
						my $prev_val_offset   = $deaths{$year}->{$week}->{$prev_age_grp}->{$sex}->{'mzcr_minus_eurostats'} // 0;
						if ($prev_val_offset < 0) {
							my $abs_offset   = abs($prev_val_offset);
							my $current_reattrib = 0;
							if ($abs_offset >= $still_missing) {
								$current_reattrib = $still_missing;
								# say "[$year] - [$week] - The offset [$still_missing] on [$age_group] can be entirely compensated.\n[$prev_from-$prev_to] : $prev_val_mzcr MZCR vs $prev_val_eurostat Eurostat";
							} else {
								$current_reattrib = $abs_offset;
								# say "[$year] - [$week] - The offset [$still_missing] on [$age_group] can be partially compensated ($abs_offset).\n[$prev_from-$prev_to] : $prev_val_mzcr MZCR vs $prev_val_eurostat Eurostat";
							}
							$deaths_to_reattrib{$sex}->{$year}->{$week}->{$age_group}->{$prev_age_grp} += $current_reattrib;

							# Subtract the deaths we just reattributed in the current group & recalcs offset.
							$deaths{$year}->{$week}->{$age_group}->{$sex}->{'mzcr_deaths'}         -= $current_reattrib;
							$deaths{$year}->{$week}->{$age_group}->{$sex}->{'mzcr_minus_eurostats'} = $deaths{$year}->{$week}->{$age_group}->{$sex}->{'mzcr_deaths'} - $deaths{$year}->{$week}->{$age_group}->{$sex}->{'eurostat_deaths'};

							# Adds the deaths to the next age group & recalcs offset.
							$deaths{$year}->{$week}->{$prev_age_grp}->{$sex}->{'mzcr_deaths'}         += $current_reattrib;
							$deaths{$year}->{$week}->{$prev_age_grp}->{$sex}->{'mzcr_minus_eurostats'} = $deaths{$year}->{$week}->{$prev_age_grp}->{$sex}->{'mzcr_deaths'} - $deaths{$year}->{$week}->{$prev_age_grp}->{$sex}->{'eurostat_deaths'};
						} else {
							# say "[$year] - [$week] - The offset [$mzcr_minus_eurostats] can't be compensated at all ($abs_offset).";
						}
					}
				}
			}
		}
	}
}

# 90-999
for my $year (sort{$a <=> $b} keys %deaths) {
	for my $week (sort{$a <=> $b} keys %{$deaths{$year}}) {
		if ($year eq 2020) {
			next if $week < 10;
		} elsif ($year eq 2024) {
			next;
		}
		for my $sex (@sexes) {
			my $eurostat_deaths      = $deaths{$year}->{$week}->{'90-999'}->{$sex}->{'eurostat_deaths'}     // 0;
			my $mzcr_deaths          = $deaths{$year}->{$week}->{'90-999'}->{$sex}->{'mzcr_deaths'}         // 0;
			my $mzcr_minus_eurostats = $deaths{$year}->{$week}->{'90-999'}->{$sex}->{'mzcr_minus_eurostats'} // 0;
			if ($mzcr_minus_eurostats > 0) {
				my $prev_val_mzcr     = $deaths{$year}->{$week}->{'85-89'}->{$sex}->{'mzcr_deaths'}         // 0;
				my $prev_val_eurostat = $deaths{$year}->{$week}->{'85-89'}->{$sex}->{'eurostat_deaths'}     // 0;
				my $prev_val_offset   = $deaths{$year}->{$week}->{'85-89'}->{$sex}->{'mzcr_minus_eurostats'} // 0;
				if ($prev_val_offset < 0) {
					my $to_reattrib;
					my $abs_offset   = abs($prev_val_offset);
					if ($abs_offset >= $mzcr_minus_eurostats) {
						$to_reattrib = $mzcr_minus_eurostats;
						say "[$year] - [$week] - The offset [$mzcr_minus_eurostats] on [90-999] can be entirely compensated ($to_reattrib). [85-89] : $prev_val_mzcr MZCR vs $prev_val_eurostat Eurostat";
					} else {
						$to_reattrib = $abs_offset;
						say "[$year] - [$week] - The offset [$mzcr_minus_eurostats] on [90-999] can be partially compensated ($to_reattrib). [85-89] : $prev_val_mzcr MZCR vs $prev_val_eurostat Eurostat";
					}

					$deaths_to_reattrib{$sex}->{$year}->{$week}->{'90-999'}->{'85-89'} += $to_reattrib;

					# Subtract the deaths we just reattributed in the current group & recalcs offset.
					$deaths{$year}->{$week}->{'90-999'}->{$sex}->{'mzcr_deaths'}         -= $to_reattrib;
					$deaths{$year}->{$week}->{'90-999'}->{$sex}->{'mzcr_minus_eurostats'} = $deaths{$year}->{$week}->{'90-999'}->{$sex}->{'mzcr_deaths'} - $deaths{$year}->{$week}->{'90-999'}->{$sex}->{'eurostat_deaths'};

					# Adds the deaths to the next age group & recalcs offset.
					$deaths{$year}->{$week}->{'85-89'}->{$sex}->{'mzcr_deaths'}         += $to_reattrib;
					$deaths{$year}->{$week}->{'85-89'}->{$sex}->{'mzcr_minus_eurostats'} = $deaths{$year}->{$week}->{'85-89'}->{$sex}->{'mzcr_deaths'} - $deaths{$year}->{$week}->{'85-89'}->{$sex}->{'eurostat_deaths'};
				}
			}
		}
	}
}

# --- Printing imputation details ---
open my $out_attrib, '>', 'outputs/imputation_layer_2_attributions.csv';
say $out_attrib "year,week,age_group_from,sex,age_group_to,to_reattrib";
for my $sex (sort keys %deaths_to_reattrib) {
	for my $year (sort{$a <=> $b} keys %{$deaths_to_reattrib{$sex}}) {
		for my $week (sort{$a <=> $b} keys %{$deaths_to_reattrib{$sex}->{$year}}) {
			for my $age_group_from (sort keys %{$deaths_to_reattrib{$sex}->{$year}->{$week}}) {
				for my $age_group_to (sort keys %{$deaths_to_reattrib{$sex}->{$year}->{$week}->{$age_group_from}}) {
					my $to_reattrib = $deaths_to_reattrib{$sex}->{$year}->{$week}->{$age_group_from}->{$age_group_to} // die;
					say $out_attrib "$year,$week,$age_group_from,$sex,$age_group_to,$to_reattrib";
				}
			}
		}
	}
}
close $out_attrib;

# --- Imputing reattributions to the MZCR layer 1 file ---
my $mzcr_total_rows = 493208;
my ($cpt, $mzcr_current_rows, $reattributed) = (0, 0, 0);
my ($mzcr_csv, $mzcr_fh, $mzcr_headers, $mzcr_idx) = open_csv_reader($mzcr_file);
open my $out_imput, '>', 'outputs/imputation_layer_2.csv';
open my $out_details, '>', 'outputs/imputation_layer_2_details.csv';
say $out_imput "id,sex,year_of_birth_end,week_date_of_death,age_at_death";
say $out_details "id,sex,year_of_birth_end,week_date_of_death,age_at_death,imputed_year_of_birth,imputed_age";
while (my $row = $mzcr_csv->getline_hr($mzcr_fh)) {
	$mzcr_current_rows++;
	$cpt++;
	if ($cpt == 1000) {
		$cpt = 0;
		STDOUT->printflush("\rParsing MZCR - [$mzcr_current_rows / $mzcr_total_rows]");
	}
	my $week_date_of_death = $row->{'week_date_of_death'};
	my $year_of_birth_end  = $row->{'year_of_birth_end'};
	my $age_at_death       = $row->{'age_at_death'}       // die;
	my $sex                = $row->{'sex'}                // die;
	my $id                 = $row->{'id'}                 // die;
	if ($year_of_birth_end && $week_date_of_death) {
		my ($year) = split '-', $week_date_of_death;
		my $week = ymd_to_iso_week($week_date_of_death);
		my ($from_year, $to_year) = from_year_to_year_from_age($age_at_death);
		my $age_group_from = "$from_year-$to_year";
		if (exists $deaths_to_reattrib{$sex}->{$year}->{$week}->{$age_group_from}) {
			my ($age_before, $yob_before) = ($age_at_death, $year_of_birth_end);
			for my $age_group_to (sort keys %{$deaths_to_reattrib{$sex}->{$year}->{$week}->{$age_group_from}}) {
				my $current = $deaths_to_reattrib{$sex}->{$year}->{$week}->{$age_group_from}->{$age_group_to} // die;
				my ($target_from, $target_to) = split '-', $age_group_to;
				if ($target_from > $from_year) {
					$year_of_birth_end = $year_of_birth_end - 5;
					$age_at_death = $age_at_death + 5;
				} else {
					$year_of_birth_end = $year_of_birth_end + 5;
					$age_at_death = $age_at_death - 5;
				}

				# Removing 1 to current
				$current = $current - 1;
				if ($current != 0) {
					$deaths_to_reattrib{$sex}->{$year}->{$week}->{$age_group_from}->{$age_group_to} = $current;
				} else {
					delete $deaths_to_reattrib{$sex}->{$year}->{$week}->{$age_group_from}->{$age_group_to};
					my $current_keys = keys %{$deaths_to_reattrib{$sex}->{$year}->{$week}->{$age_group_from}};
					if ($current_keys == 0) {
						delete $deaths_to_reattrib{$sex}->{$year}->{$week}->{$age_group_from};
					}
				}
				last;
			}
			say $out_details "$id,$sex,$yob_before,$week_date_of_death,$age_before,$year_of_birth_end,$age_at_death";
			$reattributed++;
		}
	}
	say $out_imput "$id,$sex,$year_of_birth_end,$week_date_of_death,$age_at_death";
}
close $out_imput;
close $out_details;
$mzcr_csv->eof or die "CSV error in $mzcr_file: " . $mzcr_csv->error_diag;
close $mzcr_fh or warn "Couldn't close $mzcr_file: $!";
STDOUT->printflush("\rParsing MZCR - [$mzcr_current_rows / $mzcr_total_rows]");
say "";

say "-" x 50;
say "reattributed : $reattributed";
say "-" x 50;