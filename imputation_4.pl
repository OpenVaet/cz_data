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
my $eurostat_pop_file    = "data/demo_pjan_linear_2_0.csv";
my %eurostat_pop         = ();  # {sex}{age_group} => pop
my %mzcr_pop             = ();  # {sex}{age_group} => count (known pop with YOB)
my %data_to_attribute    = ();  # {id} => {sex=>M/F/U, year_of_birth_end=>undef}
my $mzcr_file            = 'outputs/imputation_layer_3.csv';
my $mzcr_origin_file     = "data/mzcr_no_or_first_infection.csv";

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

# --------------------------------------------------
# LOADERS
# --------------------------------------------------
load_eurostat_pop();  # fills %eurostat_pop and $eurostat_total_pop (CZ, 2024, 5y+open)
load_mzcr_pop();      # fills %mzcr_pop, %data_to_attribute, $mzcr_total_pop, $yobs_missing

# For visibility: current weights (before imputation)
my %current_weights = ();
for my $sex (sort keys %mzcr_pop) {
    for my $age_group (sort keys %{$mzcr_pop{$sex}}) {
        my $mzcr_count      = $mzcr_pop{$sex}->{$age_group}     // 0;
        my $eurostat_count  = $eurostat_pop{$sex}->{$age_group} // 0;
        my $mzcr_weight     = $mzcr_total_pop > 0     ? nearest(0.0001, $mzcr_count     * 100 / $mzcr_total_pop)     : 0;
        my $eurostat_weight = $eurostat_total_pop > 0 ? nearest(0.0001, $eurostat_count * 100 / $eurostat_total_pop) : 0;
        $current_weights{$sex}->{$age_group} = {
            mzcr_pop        => $mzcr_count,
            mzcr_weight     => $mzcr_weight,
            eurostat_pop    => $eurostat_count,
            eurostat_weight => $eurostat_weight,
        };
    }
}
say "=== BEFORE IMPUTATION: MZCR vs Eurostat weights (%, total across sexes) ===";
p%current_weights;

# --------------------------------------------------
# IMPUTE UNKNOWN YOB (and sex if 'U') TO MATCH EUROSTAT WEIGHTS
# --------------------------------------------------
my $out_path = 'outputs/imputed_population_yob_2024.csv';
make_path('outputs') unless -d 'outputs';
open my $out_fh, '>:encoding(UTF-8)', $out_path or die "Can't write $out_path: $!";
say $out_fh "id,sex,year_of_birth_end,age_group";

my $n_imputed = impute_population_unknowns($out_fh);

close $out_fh or warn "Couldn't close $out_path: $!";
say "Wrote $out_path with $n_imputed imputed records.";

# --------------------------------------------------
# AFTER: recompute weights including imputed people
# --------------------------------------------------
my $final_total_pop = $mzcr_total_pop + $yobs_missing;
my %final_weights   = ();
for my $sex (@sexes) {
    for my $age_group (@age_groups) {
        my $mz = $mzcr_pop{$sex}->{$age_group} // 0;  # now includes imputed
        my $eu = $eurostat_pop{$sex}->{$age_group} // 0;
        my $mw = $final_total_pop > 0     ? nearest(0.0001, $mz * 100 / $final_total_pop) : 0;
        my $ew = $eurostat_total_pop > 0  ? nearest(0.0001, $eu * 100 / $eurostat_total_pop) : 0;
        $final_weights{$sex}->{$age_group} = {
            mzcr_pop_final  => $mz,
            mzcr_weight     => $mw,
            eurostat_pop    => $eu,
            eurostat_weight => $ew,
        };
    }
}
say "=== AFTER IMPUTATION: MZCR vs Eurostat weights (%, total across sexes) ===";
p%final_weights;

# --- Write final_weights to CSV for inspection ---
make_path('outputs') unless -d 'outputs';
my $weights_path = 'outputs/final_weights.csv';
open my $wfh, '>:encoding(UTF-8)', $weights_path or die "Can't write $weights_path: $!";

# header
$csv->print($wfh, [qw(
  sex
  age_group
  mzcr_pop_final
  mzcr_weight_pct
  eurostat_pop
  eurostat_weight_pct
  delta_pop
  delta_weight_pct
)]);

# per (sex, age_group) rows
for my $sex (@sexes) {
    for my $ag (@age_groups) {
        my $mz = $final_weights{$sex}->{$ag}->{'mzcr_pop_final'}  // 0;
        my $mw = $final_weights{$sex}->{$ag}->{'mzcr_weight'}      // 0;  # %
        my $eu = $final_weights{$sex}->{$ag}->{'eurostat_pop'}     // 0;
        my $ew = $final_weights{$sex}->{$ag}->{'eurostat_weight'}  // 0;  # %
        my $dp = $mz - $eu;
        my $dw = nearest(0.0001, $mw - $ew);
        $csv->print($wfh, [$sex, $ag, $mz, $mw, $eu, $ew, $dp, $dw]);
    }
}

# per-sex totals (age_group = ALL)
for my $sex (@sexes) {
    my ($mz_sum, $eu_sum) = (0, 0);
    for my $ag (@age_groups) {
        $mz_sum += $final_weights{$sex}->{$ag}->{'mzcr_pop_final'} // 0;
        $eu_sum += $final_weights{$sex}->{$ag}->{'eurostat_pop'}    // 0;
    }
    my $mw = $final_total_pop    > 0 ? nearest(0.0001, $mz_sum * 100 / $final_total_pop)   : 0;
    my $ew = $eurostat_total_pop > 0 ? nearest(0.0001, $eu_sum * 100 / $eurostat_total_pop): 0;
    my $dp = $mz_sum - $eu_sum;
    my $dw = nearest(0.0001, $mw - $ew);
    $csv->print($wfh, [$sex, 'ALL', $mz_sum, $mw, $eu_sum, $ew, $dp, $dw]);
}

# overall total (sex = ALL, age_group = ALL)
my ($mz_all, $eu_all) = (0, 0);
for my $sex (@sexes) {
    for my $ag (@age_groups) {
        $mz_all += $final_weights{$sex}->{$ag}->{'mzcr_pop_final'} // 0;
        $eu_all += $final_weights{$sex}->{$ag}->{'eurostat_pop'}    // 0;
    }
}
my $mw_all = $final_total_pop    > 0 ? nearest(0.0001, $mz_all * 100 / $final_total_pop)   : 0;
my $ew_all = $eurostat_total_pop > 0 ? nearest(0.0001, $eu_all * 100 / $eurostat_total_pop): 0;
my $dp_all = $mz_all - $eu_all;
my $dw_all = nearest(0.0001, $mw_all - $ew_all);
$csv->print($wfh, ['ALL', 'ALL', $mz_all, $mw_all, $eu_all, $ew_all, $dp_all, $dw_all]);

close $wfh or warn "Couldn't close $weights_path: $!";
say "Wrote $weights_path";


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

sub load_eurostat_pop {
    my ($csv_r, $fh, $headers, $idx) = open_csv_reader($eurostat_pop_file);
    while (my $row = $csv_r->getline_hr($fh)) {
        my $sex   = $row->{'sex'}       // die;
        my $geo   = $row->{'geo'}       // die;
        my $value = $row->{'OBS_VALUE'} // die;
        my $year  = $row->{'TIME_PERIOD'} // die;
        next unless $geo eq 'CZ';
        next unless $sex eq 'M' || $sex eq 'F';
        next unless $year == 2024;

        my $age = $row->{'age'} // die;
        next if $age eq 'TOTAL';

        my ($from_year, $to_year);
        if ($age eq 'UNK') {
            die if $value;
            next;
        } elsif ($age eq 'Y_LT1') {
            ($from_year, $to_year) = (0, 4);
        } elsif ($age eq 'Y_OPEN') {
            ($from_year, $to_year) = (90, 999);
        } else {
            if ($age =~ /Y/) {
                $age =~ s/Y//;
                die unless looks_like_number($age);
                if    ($age <= 4)  { ($from_year,$to_year)=(0,4)   }
                elsif ($age <= 9)  { ($from_year,$to_year)=(5,9)   }
                elsif ($age <= 14) { ($from_year,$to_year)=(10,14) }
                elsif ($age <= 19) { ($from_year,$to_year)=(15,19) }
                elsif ($age <= 24) { ($from_year,$to_year)=(20,24) }
                elsif ($age <= 29) { ($from_year,$to_year)=(25,29) }
                elsif ($age <= 34) { ($from_year,$to_year)=(30,34) }
                elsif ($age <= 39) { ($from_year,$to_year)=(35,39) }
                elsif ($age <= 44) { ($from_year,$to_year)=(40,44) }
                elsif ($age <= 49) { ($from_year,$to_year)=(45,49) }
                elsif ($age <= 54) { ($from_year,$to_year)=(50,54) }
                elsif ($age <= 59) { ($from_year,$to_year)=(55,59) }
                elsif ($age <= 64) { ($from_year,$to_year)=(60,64) }
                elsif ($age <= 69) { ($from_year,$to_year)=(65,69) }
                elsif ($age <= 74) { ($from_year,$to_year)=(70,74) }
                elsif ($age <= 79) { ($from_year,$to_year)=(75,79) }
                elsif ($age <= 84) { ($from_year,$to_year)=(80,84) }
                elsif ($age <= 89) { ($from_year,$to_year)=(85,89) }
                else               { ($from_year,$to_year)=(90,999) }
            } else {
                die;
            }
        }
        my $ag = "$from_year-$to_year";
        $eurostat_pop{$sex}->{$ag} += $value;
        $eurostat_total_pop += $value;
    }
    $csv_r->eof or die "CSV error in $eurostat_pop_file: " . $csv_r->error_diag;
    close $fh or warn "Couldn't close $eurostat_pop_file: $!";
}

sub load_mzcr_pop {
    my $total_rows = 12125969;
    my ($cpt, $cur) = (0, 0);
    my ($csv_r, $fh, $headers, $idx) = open_csv_reader($mzcr_origin_file);

    while (my $row = $csv_r->getline_hr($fh)) {
        $cur++; $cpt++;
        if ($cpt == 1000) { $cpt = 0; STDOUT->printflush("\rParsing MZCR - [$cur / $total_rows]"); }

        my $id                 = $row->{'ID'}                 // die;
        my $week_date_of_death = $row->{'week_date_of_death'} // die;
        my $year_of_birth_end  = $row->{'year_of_birth_end'};
        my $gender             = $row->{'gender'};  # 1,2 or empty
        my $sex = ($gender && $gender == 1) ? 'M' : ($gender && $gender == 2) ? 'F' : 'U';

        # We only consider people with NO death date (i.e., in the "population" pool).
        if (!$week_date_of_death) {
            if (defined $year_of_birth_end && $year_of_birth_end ne '') {
                my $age_2024 = 2024 - $year_of_birth_end - 1;
                $age_2024 = 0 if $age_2024 < 0;
                my ($from_year, $to_year) = from_year_to_year_from_age($age_2024);
                $mzcr_pop{$sex}->{"$from_year-$to_year"}++;
                $mzcr_total_pop++;
            } else {
                # Missing YOB => we will impute it (and sex if 'U')
                $yobs_missing++;
                $data_to_attribute{$id}->{'sex'} = $sex;
                $data_to_attribute{$id}->{'year_of_birth_end'} = undef;
            }
        }
    }
    $csv_r->eof or die "CSV error in $mzcr_origin_file: " . $csv_r->error_diag;
    close $fh or warn "Couldn't close $mzcr_origin_file: $!";
    STDOUT->printflush("\rParsing MZCR - [$cur / $total_rows]"); say "";
}

# ---------------- apportion helper (largest remainder, integer) ----------------
sub apportion_with_caps {
    my ($total, $items) = @_;  # [ [ key, weight, cap_int ], ... ]
    my %alloc = map { $_->[0] => 0 } @$items;
    return %alloc if $total <= 0 || !@$items;

    # keep only items with positive caps
    my @rows = grep { $_->[2] > 0 } @$items;
    return %alloc unless @rows;

    my $sum_w = 0; $sum_w += $_->[1] for @rows;

    # If total weight is 0, just fill in declared order up to caps
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

    my $allocated = 0;
    my @q;
    for my $r (@rows) {
        my ($key,$w,$cap) = @$r;            # cap must be integer
        my $q = $total * ($w / $sum_w);     # real quota
        my $f = int($q);
        $f = $cap if $f > $cap;             # respect cap
        push @q, [$key,$w,$cap,$q,$f,($q-$f)];
        $allocated += $f;
    }

    my $remain = $total - $allocated;
    if ($remain > 0) {
        # grant remaining units by largest fractional part, still respecting caps
        for my $row (sort { $b->[5] <=> $a->[5] } @q) {
            last if $remain <= 0;
            next if $row->[4] >= $row->[2];  # at cap
            $row->[4]++; $remain--;
        }
    } elsif ($remain < 0) {
        # remove from smallest fractional parts
        for my $row (sort { $a->[5] <=> $b->[5] } @q) {
            last if $remain >= 0;
            next if $row->[4] <= 0;
            $row->[4]--; $remain++;
        }
    }
    %alloc = map { $_->[0] => $_->[4] } @q;
    return %alloc;
}

# ---------------- main imputation ----------------
sub impute_population_unknowns {
    my ($out_fh) = @_;

    # Collect unknown IDs by sex flag (M-only, F-only, U)
    my (@ids_M, @ids_F, @ids_U);
    for my $id (keys %data_to_attribute) {
        my $sx = $data_to_attribute{$id}->{'sex'} // 'U';
        if    ($sx eq 'M') { push @ids_M, $id; }
        elsif ($sx eq 'F') { push @ids_F, $id; }
        else               { push @ids_U, $id; }
    }

    my $U_total  = scalar @ids_U;
    my $fix_M    = scalar @ids_M;   # sex-fixed M (YOB unknown)
    my $fix_F    = scalar @ids_F;   # sex-fixed F (YOB unknown)
    my $final_total = $mzcr_total_pop + $yobs_missing;

    # Current known totals by sex (with YOB already known)
    my %K_sex_total = ();
    for my $sex (@sexes) {
        my $sum = 0;
        for my $ag (@age_groups) {
            $sum += $mzcr_pop{$sex}->{$ag} // 0;
        }
        $K_sex_total{$sex} = $sum;
    }

    # Eurostat totals by sex
    my %E_sex_total = ();
    for my $sex (@sexes) {
        my $sum = 0;
        for my $ag (@age_groups) {
            $sum += $eurostat_pop{$sex}->{$ag} // 0;
        }
        $E_sex_total{$sex} = $sum;
    }

    # Target final sex totals (shares applied to final_total)
    my %target_sex_final;
    for my $sex (@sexes) {
        my $share = $eurostat_total_pop > 0 ? ($E_sex_total{$sex} / $eurostat_total_pop) : 0;
        $target_sex_final{$sex} = $final_total * $share; # keep as real; rounding handled by apportion
    }

    # Sex deficits after placing sex-fixed unknowns into their sex
    my $need_M = $target_sex_final{'M'} - ($K_sex_total{'M'} + $fix_M);
    my $need_F = $target_sex_final{'F'} - ($K_sex_total{'F'} + $fix_F);
    $need_M = 0 if $need_M < 0;
    $need_F = 0 if $need_F < 0;

    # Stage S1: allocate U across sexes to fill sex deficits first
    my $cap_M = int($need_M + 0.999999);
    my $cap_F = int($need_F + 0.999999);
    my $cap_sum = $cap_M + $cap_F;
    my %allocU = ();
    my $alloc_fromU = 0;

    if ($cap_sum > 0) {
        my $t1 = $U_total < $cap_sum ? $U_total : $cap_sum;
        %allocU = apportion_with_caps($t1, [
            [ 'M', $need_M, $cap_M ],
            [ 'F', $need_F, $cap_F ],
        ]);
        $alloc_fromU = $allocU{'M'} + $allocU{'F'};
    } else {
        %allocU = ( 'M' => 0, 'F' => 0 );
    }

    # Stage S2: leftover U (if any) by Eurostat sex shares
    my $leftU = $U_total - $alloc_fromU;
    if ($leftU > 0) {
        my %allocU2 = apportion_with_caps($leftU, [
            [ 'M', ($E_sex_total{'M'} > 0 ? $E_sex_total{'M'} : 1), $leftU ],
            [ 'F', ($E_sex_total{'F'} > 0 ? $E_sex_total{'F'} : 1), $leftU ],
        ]);
        $allocU{'M'} += $allocU2{'M'} // 0;
        $allocU{'F'} += $allocU2{'F'} // 0;
    }

    # Move that many U-ids into each sex’s list
    my $moveM = $allocU{'M'} // 0;
    my $moveF = $allocU{'F'} // 0;
    while ($moveM-- > 0 && @ids_U) { push @ids_M, shift @ids_U; }
    while ($moveF-- > 0 && @ids_U) { push @ids_F, shift @ids_U; }
    # any residual U due to rounding: just balance to shorter side
    while (@ids_U) {
        if (@ids_M <= @ids_F) { push @ids_M, shift @ids_U; }
        else                  { push @ids_F, shift @ids_U; }
    }

    # Now, per sex, allocate unknowns across age groups
    my $imputed = 0;

    for my $sex ('M','F') {
        my @ids = $sex eq 'M' ? @ids_M : @ids_F;
        my $U_s = scalar @ids;
        next if $U_s <= 0;

        # Known counts per age group for this sex
        my %K_sg = map { $_ => ($mzcr_pop{$sex}->{$_} // 0) } @age_groups;
        my $K_s_total = $K_sex_total{$sex} // 0;

        my $E_s_total = $E_sex_total{$sex} // 0;
        # If Eurostat has zero for this sex (shouldn't happen), avoid div/0
        $E_s_total = 1 if $E_s_total <= 0;

        my $final_s_total = $K_s_total + $U_s;

        # First pass: fill deficits relative to per-sex Eurostat pattern
        my @need_items; my $need_cap_sum = 0;
        for my $ag (@age_groups) {
            my $w = $eurostat_pop{$sex}->{$ag} // 0;
            my $target_sg = $final_s_total * ($w / $E_s_total);
            my $need = $target_sg - $K_sg{$ag};
            if ($need > 0) {
                my $cap = int($need + 0.999999); # ceil
                push @need_items, [ $ag, $need, $cap ];
                $need_cap_sum += $cap;
            }
        }
        my %alloc1 = ();
        my $t1 = $U_s < $need_cap_sum ? $U_s : $need_cap_sum;
        if ($t1 > 0 && @need_items) {
            %alloc1 = apportion_with_caps($t1, \@need_items);
        } else {
            %alloc1 = ();
        }

        my $assigned1 = 0; $assigned1 += $_ for values %alloc1;
        my $left = $U_s - $assigned1;

        # Second pass: leftover by Eurostat per-sex age weights
        my %alloc2 = ();
        if ($left > 0) {
            my @items2 = map {
                my $w = $eurostat_pop{$sex}->{$_} // 0;
                [ $_, ($w > 0 ? $w : 1), $left ]
            } @age_groups;
            %alloc2 = apportion_with_caps($left, \@items2);
        }

        # Sum per-age allocations and write IDs
        my %take_per_ag = map { $_ => 0 } @age_groups;
        $take_per_ag{$_} += $alloc1{$_} // 0 for keys %alloc1;
        $take_per_ag{$_} += $alloc2{$_} // 0 for keys %alloc2;

        # Assign IDs to age groups (ascending) and write rows
        for my $ag (sort {
                my ($af,$at)=split/-/,$a; my ($bf,$bt)=split/-/,$b; ($af<=>$bf)||($at<=>$bt)
            } @age_groups) {
            my $take = $take_per_ag{$ag} // 0;
            next if $take <= 0;
            my ($from_year) = split '-', $ag;
            for (1..$take) {
                my $id = shift @ids; last unless defined $id;
                my $yobe = 2024 - ($from_year + 1); # "year_of_birth_end"
                my $age  = 2024 - $yobe;            # equals $from_year
                print $out_fh join(',', $id, $sex, $yobe, $ag) . "\n";
                # Update in-memory counts so final weights include imputed
                $mzcr_pop{$sex}->{$ag}++;
                $imputed++;
            }
        }
    }

    return $imputed;
}
