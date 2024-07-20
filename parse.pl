#!/usr/bin/perl
use strict;
use warnings;
use 5.30.0;
no autovivification;
binmode STDOUT, ":utf8";
use utf8;
use Data::Printer;
use Data::Dumper;
use FindBin;
use lib "$FindBin::Bin/../lib";
use Text::CSV;
use Math::Round qw(nearest);
use DateTime;

my %stats       = ();
my $cutoff_year = 2022;
my $cutoff_date = DateTime->new(
    year        => $cutoff_year,
    month       => '12',
    day         => '31'
);

parse_data();
print_by_brand();
print_by_brand_summary();
print_by_brand_age_summary();
print_cutoff_population();
print_dose_1_by_brand_month();
print_population_year_birth_sex();
print_vaccinated_population_year_birth_sex();
print_unvaccinated_population_year_birth_sex();
print_first_doses_administered_by_year_month();
print_doses_administered_by_year_month();
print_first_doses_administered_by_brand_lot();
print_first_doses_administered_by_lot_brand();

sub parse_data {
    my $source_file = "raw_data/Vesely_106_202403141131.csv";
    my $csv = Text::CSV->new({ binary => 1, auto_diag => 1, sep_char => ',' });

    open my $in, '<:encoding(utf8)', $source_file or die $!;
    open my $out, '>:encoding(utf8)', 'data/deaths_only.csv' or die $!;
    
    my ($r_num, $deaths, $latest_death, $earliest_death) = (0, 0, 0, 99999999);
    
    while (my $row = $csv->getline($in)) {
        $r_num++;
        my ($sex_code, $year_birth, $date_of_death, @vaccination_data) = @$row;

        my $birth_date;
        if ($year_birth ne 'Rok_narozeni') {
	        $birth_date = DateTime->new(
	            year  => $year_birth,
	            month => 12,
	            day   => 31
	        );
	    }
        my $age_at_death;
        if ($date_of_death && $year_birth ne 'Rok_narozeni') {
            $deaths++;

            my $comp_death = $date_of_death;
            $comp_death =~ s/\D//g;
            if ($comp_death < $earliest_death) { $earliest_death = $comp_death }
            if ($comp_death > $latest_death) { $latest_death = $comp_death }
            
            # Calculate age at death
            my ($death_year, $death_month, $death_day) = split('-', $date_of_death);
			die if $death_year < $year_birth;
            my $death_date = DateTime->new(
                year  => $death_year,
                month => $death_month,
                day   => $death_day
            );
            $age_at_death = $death_date->year - $birth_date->year;
            $age_at_death-- if ($death_year == $year_birth && ($death_date->month < $birth_date->month) || ($death_date->month == $birth_date->month && $death_date->day < $birth_date->day));
            $age_at_death = 0 if $age_at_death < 0;

            # Add age_at_death to the row
            push @$row, $age_at_death;
            
            $csv->print($out, $row);
            print $out "\n";
        	# last if $r_num > 10000;  ###################### DEBUG
        } elsif ($date_of_death) {
            # Add age_at_death to the row
            push @$row, 'age_at_death';
            
            $csv->print($out, $row);
            print $out "\n";
        }

        # Parses dose data.
        my ($at_least_one_dose, $first_dose_date, $first_dose_brand) = (0, undef, undef);
        for my $dose (0 .. 6) {
        	my $dose_date_n    = $dose * 4;
        	my $lot_n          = $dose * 4 + 1;
        	my $lot_ref_n      = $dose * 4 + 2;
        	my $lot_brand_n    = $dose * 4 + 3;
        	my $dose_date      = $vaccination_data[$dose_date_n] || last;
        	next if $dose_date =~ /Datum_/;
        	$at_least_one_dose = 1;
        	my $lot            = uc $vaccination_data[$lot_n]    // die;
        	$lot               =~ s/ //g;
        	my $lot_ref        = $vaccination_data[$lot_ref_n]   // die;
        	my $lot_brand      = $vaccination_data[$lot_brand_n] // die;
			my $dose_type_norm = normalize_dose($lot_brand);
        	my ($dose_year, $dose_month, $dose_day) = split "-", $dose_date;
        	unless ($first_dose_brand) {
	        	$first_dose_brand  = $lot_brand;
	        	$first_dose_date   = $dose_date;
        		$stats{'first_doses_administered_by_year_month'}->{$dose_year}->{$dose_month}->{$dose_type_norm}++;
        		$stats{'first_doses_administered_by_brand_lot'}->{$dose_type_norm}->{$lot}++;
        		$stats{'first_doses_administered_by_lot_brand'}->{$lot}->{$dose_type_norm}->{'total'}++;
        		$stats{'first_doses_administered_by_lot_brand'}->{$lot}->{$dose_type_norm}->{'deaths'}++ if ($age_at_death);
        	}
        	die "dose_date : $dose_date" if !$dose_month;
        	$stats{'doses_administered_by_year_month'}->{$dose_year}->{$dose_month}->{$dose_type_norm}++;
        }

        if ($year_birth ne 'Rok_narozeni') {
	        my $age_at_cutoff;
	        if ($age_at_death) {
	        	if ($first_dose_date) {
	        		my $first_dose_date_cp = $first_dose_date;
	        		my $death_date_cp = $date_of_death;
	        		$first_dose_date_cp =~ s/\D//g;
	        		$death_date_cp =~ s/\D//g;
	        		die if $death_date_cp < $first_dose_date_cp;
	        	}

	        	$age_at_cutoff = $age_at_death;
	        } else {
	            $age_at_cutoff = $cutoff_date->year - $birth_date->year;
	            $age_at_cutoff-- if ($cutoff_year == $year_birth && ($cutoff_date->month < $birth_date->month) || ($cutoff_date->month == $birth_date->month && $cutoff_date->day < $birth_date->day));
	            $age_at_cutoff = 0 if $age_at_cutoff < 0;
	        	$stats{'population_at_cutoff'}->{$age_at_cutoff}++;
	        }
	        if ($at_least_one_dose) {
	        	my $first_dose_compdate = $first_dose_date;
	        	$first_dose_compdate =~ s/\D//g;
	        	if ($first_dose_compdate <= 20221231) {
		        	my ($first_dose_year, $first_dose_month, $first_dose_day) = split "-", $first_dose_date;
					my $first_dose_type_norm = normalize_dose($first_dose_brand);
		        	$stats{'first_doses_admistered'}->{$first_dose_type_norm}->{"$first_dose_year-$first_dose_month"}->{$age_at_cutoff}++;
		        	$stats{'dose_1_data'}->{$first_dose_brand}++;
		        	$stats{'dose_1_data_by_age'}->{$first_dose_type_norm}->{$age_at_cutoff}++;
	        	} else {
		        	$stats{'dose_1_data'}->{'Unvaccinated'}++;
		        	$stats{'dose_1_data_by_age'}->{'Unvaccinated'}->{$age_at_cutoff}++;
		        }
	        	$stats{'vaccinated_population_year_birth_sex'}->{$year_birth}->{$sex_code}++;
	        } else {
	        	$stats{'unvaccinated_population_year_birth_sex'}->{$year_birth}->{$sex_code}++;
	        	$stats{'dose_1_data'}->{'Unvaccinated'}++;
	        	$stats{'dose_1_data_by_age'}->{'Unvaccinated'}->{$age_at_cutoff}++;
	        }
	        $stats{'population_year_birth_sex'}->{$year_birth}->{$sex_code}++;
        }
    }
    
    close $out or die $!;
    close $in or die $!;

    say "deaths         : $deaths";
    say "r_num          : $r_num";
    say "earliest_death : $earliest_death";
    say "latest_death   : $latest_death";
    $stats{'global_stats'}->{'total_subjects'} = $r_num;
    $stats{'global_stats'}->{'total_deaths'} = $deaths;
    $stats{'global_stats'}->{'total_not_dead'} = $stats{'global_stats'}->{'total_subjects'} - $deaths;

}

sub print_by_brand {

	# Printing summary by brand.
    open my $out, '>:encoding(utf8)', 'data/doses_1_by_brand.csv' or die $!;
    say $out "first_dose_type,total_recipients";
	for my $first_dose_type (sort keys %{$stats{'dose_1_data'}}) {
		my $value = $stats{'dose_1_data'}->{$first_dose_type} // die;
		my $first_dose_type_norm = normalize_dose($first_dose_type);
    	$stats{'dose_1_data_norm'}->{$first_dose_type_norm} += $value;
    	say $out "$first_dose_type,$value";
	}
    close $out or die $!;
}

sub normalize_dose {
	my $first_dose_type = lc shift;
	my $first_dose_type_norm;
	if ($first_dose_type =~ /comirnaty/) {
		$first_dose_type_norm = 'Comirnaty';
	} elsif ($first_dose_type =~ /janssen/) {
		$first_dose_type_norm = 'Janssen';
	} elsif ($first_dose_type =~ /nuvaxovid/) {
		$first_dose_type_norm = 'Novavax';
	} elsif ($first_dose_type =~ /spikevax/) {
		$first_dose_type_norm = 'Spikevax';
	} elsif ($first_dose_type =~ /unvaccinated/) {
		$first_dose_type_norm = 'Unvaccinated';
	} elsif ($first_dose_type =~ /vaxzevria/) {
		$first_dose_type_norm = 'Astra-Zeneca';
	} else {
		$first_dose_type_norm = 'Others';
	}
	return $first_dose_type_norm;
}

sub print_by_brand_summary {

	# Printing summary by brand summarized.
    open my $out, '>:encoding(utf8)', 'data/doses_1_by_brand_summarized.csv' or die $!;
    say $out "first_dose_type,total_recipients";
	for my $first_dose_type (sort keys %{$stats{'dose_1_data_norm'}}) {
		my $value = $stats{'dose_1_data_norm'}->{$first_dose_type} // die;
    	say $out "$first_dose_type,$value";
	}
    close $out or die $!;
}

sub print_by_brand_age_summary {

	# Printing summary by brand summarized.
    open my $out, '>:encoding(utf8)', 'data/doses_1_by_brand_age_summarized.csv' or die $!;
    say $out "first_dose_type_norm,age_at_cutoff,total_recipients";
	for my $first_dose_type_norm (sort keys %{$stats{'dose_1_data_by_age'}}) {
		for my $age_at_cutoff (sort keys %{$stats{'dose_1_data_by_age'}->{$first_dose_type_norm}}) {
			my $value = $stats{'dose_1_data_by_age'}->{$first_dose_type_norm}->{$age_at_cutoff} // die;
	    	say $out "$first_dose_type_norm,$age_at_cutoff,$value";
		}
	}
    close $out or die $!;
}

sub print_cutoff_population {
    open my $out, '>:encoding(utf8)', 'data/population_end_2022.csv' or die $!;
    say $out "age_at_cutoff,population";
	for my $age_at_cutoff (sort{$a <=> $b} keys %{$stats{'population_at_cutoff'}}) {
		my $population = $stats{'population_at_cutoff'}->{$age_at_cutoff} // die;
    	say $out "$age_at_cutoff,$population";
	}
    close $out or die $!;
}

sub print_dose_1_by_brand_month {
    open my $out, '>:encoding(utf8)', 'data/dose_1_by_brand_age_month.csv' or die $!;
    say $out "first_dose_type_norm,year_month,age_at_cutoff,total_recipients";
	for my $first_dose_type_norm (sort keys %{$stats{'first_doses_admistered'}}) {
		for my $year_month (sort keys %{$stats{'first_doses_admistered'}->{$first_dose_type_norm}}) {
			for my $age_at_cutoff (sort keys %{$stats{'first_doses_admistered'}->{$first_dose_type_norm}->{$year_month}}) {
				my $total_recipients = $stats{'first_doses_admistered'}->{$first_dose_type_norm}->{$year_month}->{$age_at_cutoff} // die;
    			say $out "$first_dose_type_norm,$year_month,$age_at_cutoff,$total_recipients";
			}
		}
	}
    close $out or die $!;
}

sub print_population_year_birth_sex  {
    open my $out, '>:encoding(utf8)', 'data/population_year_birth_sex.csv' or die $!;
    say $out "year_birth,male_population,female_population,population";
	for my $year_birth (sort{$a <=> $b} keys %{$stats{'population_year_birth_sex'}}) {
		my $male_population   = $stats{'population_year_birth_sex'}->{$year_birth}->{'M'} // 0;
		my $female_population = $stats{'population_year_birth_sex'}->{$year_birth}->{'F'} // 0;
		my $population        = $male_population + $female_population;
		say $out "$year_birth,$male_population,$female_population,$population";
	}
    close $out or die $!;
}

sub print_vaccinated_population_year_birth_sex  {
    open my $out, '>:encoding(utf8)', 'data/vaccinated_population_year_birth_sex.csv' or die $!;
    say $out "year_birth,male_population,female_population,population";
	for my $year_birth (sort{$a <=> $b} keys %{$stats{'vaccinated_population_year_birth_sex'}}) {
		my $male_population   = $stats{'vaccinated_population_year_birth_sex'}->{$year_birth}->{'M'} // 0;
		my $female_population = $stats{'vaccinated_population_year_birth_sex'}->{$year_birth}->{'F'} // 0;
		my $population        = $male_population + $female_population;
		say $out "$year_birth,$male_population,$female_population,$population";
	}
    close $out or die $!;
}

sub print_unvaccinated_population_year_birth_sex  {
    open my $out, '>:encoding(utf8)', 'data/unvaccinated_population_year_birth_sex.csv' or die $!;
    say $out "year_birth,male_population,female_population,population";
	for my $year_birth (sort{$a <=> $b} keys %{$stats{'unvaccinated_population_year_birth_sex'}}) {
		my $male_population   = $stats{'unvaccinated_population_year_birth_sex'}->{$year_birth}->{'M'} // 0;
		my $female_population = $stats{'unvaccinated_population_year_birth_sex'}->{$year_birth}->{'F'} // 0;
		my $population        = $male_population + $female_population;
		say $out "$year_birth,$male_population,$female_population,$population";
	}
    close $out or die $!;
}

sub print_first_doses_administered_by_year_month {
    open my $out_1, '>:encoding(utf8)', 'data/first_doses_administered_by_year_month.csv' or die $!;
    say $out_1 "dose_year,dose_month,comirnaty,janssen,novavax,spikevax,astrazeneca,others";
    open my $out_2, '>:encoding(utf8)', 'data/first_doses_percents_administered_by_year_month.csv' or die $!;
    say $out_2 "dose_year,dose_month,comirnaty,janssen,novavax,spikevax,astrazeneca,others";
	for my $dose_year (sort{$a <=> $b} keys %{$stats{'first_doses_administered_by_year_month'}}) {
		for my $dose_month (sort{$a <=> $b} keys %{$stats{'first_doses_administered_by_year_month'}->{$dose_year}}) {
			my $comirnaty   = $stats{'first_doses_administered_by_year_month'}->{$dose_year}->{$dose_month}->{'Comirnaty'}    // 0;
			my $janssen     = $stats{'first_doses_administered_by_year_month'}->{$dose_year}->{$dose_month}->{'Janssen'}      // 0;
			my $novavax     = $stats{'first_doses_administered_by_year_month'}->{$dose_year}->{$dose_month}->{'Novavax'}      // 0;
			my $spikevax    = $stats{'first_doses_administered_by_year_month'}->{$dose_year}->{$dose_month}->{'Spikevax'}     // 0;
			my $astrazeneca = $stats{'first_doses_administered_by_year_month'}->{$dose_year}->{$dose_month}->{'Astra-Zeneca'} // 0;
			my $others      = $stats{'first_doses_administered_by_year_month'}->{$dose_year}->{$dose_month}->{'Others'}       // 0;
			my $total_monthly_doses = $comirnaty + $janssen + $novavax + $spikevax + $astrazeneca + $others;
			my $comirnaty_percent   = nearest(0.01, $comirnaty   * 100 / $total_monthly_doses);
			my $janssen_percent     = nearest(0.01, $janssen     * 100 / $total_monthly_doses);
			my $novavax_percent     = nearest(0.01, $novavax     * 100 / $total_monthly_doses);
			my $spikevax_percent    = nearest(0.01, $spikevax    * 100 / $total_monthly_doses);
			my $astrazeneca_percent = nearest(0.01, $astrazeneca * 100 / $total_monthly_doses);
			my $others_percent      = nearest(0.01, $others      * 100 / $total_monthly_doses);
    		say $out_1 "$dose_year,$dose_month,$comirnaty,$janssen,$novavax,$spikevax,$astrazeneca,$others";
    		say $out_2 "$dose_year,$dose_month,$comirnaty_percent,$janssen_percent,$novavax_percent,$spikevax_percent,$astrazeneca_percent,$others_percent";
		}
	}
    close $out_1 or die $!;
    close $out_2 or die $!;
}

sub print_doses_administered_by_year_month {
    open my $out_1, '>:encoding(utf8)', 'data/doses_administered_by_year_month.csv' or die $!;
    say $out_1 "dose_year,dose_month,comirnaty,janssen,novavax,spikevax,astrazeneca,others";
    open my $out_2, '>:encoding(utf8)', 'data/doses_percents_administered_by_year_month.csv' or die $!;
    say $out_2 "dose_year,dose_month,comirnaty,janssen,novavax,spikevax,astrazeneca,others";
	for my $dose_year (sort{$a <=> $b} keys %{$stats{'doses_administered_by_year_month'}}) {
		for my $dose_month (sort{$a <=> $b} keys %{$stats{'doses_administered_by_year_month'}->{$dose_year}}) {
			my $comirnaty   = $stats{'doses_administered_by_year_month'}->{$dose_year}->{$dose_month}->{'Comirnaty'}    // 0;
			my $janssen     = $stats{'doses_administered_by_year_month'}->{$dose_year}->{$dose_month}->{'Janssen'}      // 0;
			my $novavax     = $stats{'doses_administered_by_year_month'}->{$dose_year}->{$dose_month}->{'Novavax'}      // 0;
			my $spikevax    = $stats{'doses_administered_by_year_month'}->{$dose_year}->{$dose_month}->{'Spikevax'}     // 0;
			my $astrazeneca = $stats{'doses_administered_by_year_month'}->{$dose_year}->{$dose_month}->{'Astra-Zeneca'} // 0;
			my $others      = $stats{'doses_administered_by_year_month'}->{$dose_year}->{$dose_month}->{'Others'}       // 0;
			my $total_monthly_doses = $comirnaty + $janssen + $novavax + $spikevax + $astrazeneca + $others;
			my $comirnaty_percent   = nearest(0.01, $comirnaty   * 100 / $total_monthly_doses);
			my $janssen_percent     = nearest(0.01, $janssen     * 100 / $total_monthly_doses);
			my $novavax_percent     = nearest(0.01, $novavax     * 100 / $total_monthly_doses);
			my $spikevax_percent    = nearest(0.01, $spikevax    * 100 / $total_monthly_doses);
			my $astrazeneca_percent = nearest(0.01, $astrazeneca * 100 / $total_monthly_doses);
			my $others_percent      = nearest(0.01, $others      * 100 / $total_monthly_doses);
    		say $out_1 "$dose_year,$dose_month,$comirnaty,$janssen,$novavax,$spikevax,$astrazeneca,$others";
    		say $out_2 "$dose_year,$dose_month,$comirnaty_percent,$janssen_percent,$novavax_percent,$spikevax_percent,$astrazeneca_percent,$others_percent";
		}
	}
    close $out_1 or die $!;
    close $out_2 or die $!;
}

sub print_first_doses_administered_by_brand_lot {
    open my $out, '>:encoding(utf8)', 'data/first_doses_administered_by_brand_lot.csv' or die $!;
    say $out "dose_type_norm,lot,total_recipients";
	for my $dose_type_norm (sort keys %{$stats{'first_doses_administered_by_brand_lot'}}) {
		for my $lot (sort keys %{$stats{'first_doses_administered_by_brand_lot'}->{$dose_type_norm}}) {
			my $total_recipients = $stats{'first_doses_administered_by_brand_lot'}->{$dose_type_norm}->{$lot} // die;
			say $out "$dose_type_norm,$lot,$total_recipients";
		}
	}
    close $out or die $!;
}

sub print_first_doses_administered_by_lot_brand {
    open my $out, '>:encoding(utf8)', 'data/first_doses_administered_by_lot_brand.csv' or die $!;
    say $out "lot,dose_type_norm,total_recipients";
    my ($total_misclassified, $total_misclassified_deaths) = (0, 0, 0);
	for my $lot (sort keys %{$stats{'first_doses_administered_by_lot_brand'}}) {
		my $total_brands = keys %{$stats{'first_doses_administered_by_lot_brand'}->{$lot}};
		if ($total_brands > 1) {
			my ($highest, $misclassified, $deaths) = (0, 0, 0);
			for my $dose_type_norm (sort keys %{$stats{'first_doses_administered_by_lot_brand'}->{$lot}}) {
				my $total_recipients = $stats{'first_doses_administered_by_lot_brand'}->{$lot}->{$dose_type_norm}->{'total'} // die;
				$highest = $total_recipients if $highest < $total_recipients;
				say $out "$lot,$dose_type_norm,$total_recipients";
			}
			for my $dose_type_norm (sort keys %{$stats{'first_doses_administered_by_lot_brand'}->{$lot}}) {
				my $total_recipients = $stats{'first_doses_administered_by_lot_brand'}->{$lot}->{$dose_type_norm}->{'total'} // die;
				if ($total_recipients != $highest) {
					my $total_deaths = $stats{'first_doses_administered_by_lot_brand'}->{$lot}->{$dose_type_norm}->{'deaths'} // 0;
					$misclassified += $total_recipients;
					$deaths += $total_deaths;
				}
			}
			$total_misclassified += $misclassified;
			$total_misclassified_deaths += $deaths;
		}
	}
    close $out or die $!;


    my $total_subjects = $stats{'global_stats'}->{'total_subjects'} // die;
    my $total_deaths = $stats{'global_stats'}->{'total_deaths'} // die;
    my $total_not_dead = $stats{'global_stats'}->{'total_not_dead'} // die;
    my $total_not_misclassified = $total_subjects - $total_misclassified;
    my $total_not_misclassified_deaths = $total_deaths - $total_misclassified_deaths;
    say "total_misclassified            : $total_misclassified";
    say "total_misclassified_deaths     : $total_misclassified_deaths";
    say "total_not_misclassified        : $total_not_misclassified";
    say "total_not_misclassified_deaths : $total_not_misclassified_deaths";
}

