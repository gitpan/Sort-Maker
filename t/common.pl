use strict ;

use Data::Dumper ;

use Test::More ;
use Benchmark ;

use Sort::Maker qw( :all ) ;

sub test_driver {

	my( $sort_tests, $default_styles ) = @_ ;

	my $total_tests = count_tests( $sort_tests, $default_styles ) ;

	plan tests => $total_tests ;

	foreach my $test ( @{$sort_tests} ) {

		if ( $test->{skip} ) {

########
# calc number of sorts in this test
########

			SKIP: {
				skip( "sort of $test->{name}\n",
					$test->{count} ) ;
			}
			next ;
		}

		$test->{data} ||= generate_data( $test ) ;

		make_test_sorters( $test, $default_styles ) ;

		run_tests( $test ) ;
	}
}

sub run_tests {

	my( $test ) = @_ ;

	my $input = $test->{data} ;

	my @gold_sorted = sort { $test->{gold}->() } @{$input} ;

	foreach my $sort_name ( sort test_name_cmp keys %{$test->{sorters}} ) {

		my @sorter_in = $sort_name =~ /ref_in/ ? $input : @{$input} ;

		my $sorter = $test->{sorters}{$sort_name} ;
		my @test_sorted = $sorter->( @sorter_in ) ;
		@test_sorted = @{$test_sorted[0]} if $sort_name =~ /ref_out/ ;

#print "style SORTED [@test_sorted]\n" ;

		ok( eq_array( \@gold_sorted, \@test_sorted ),
			"$sort_name sort of $test->{name}" ) ;
	}
}

sub test_name_cmp {

	my @a = split /_/, $a ;
	my @b = split /_/, $b ;

	lc $a[0] cmp lc $b[0]
		||
	lc $a[1] cmp lc $b[1]
		||
	lc $a[2] cmp lc $b[2]
}

sub benchmark_driver {

	my( $sort_tests, $default_styles, $default_sizes ) = @_ ;

	my $duration = shift @ARGV || -2 ;

	foreach my $test ( @{$sort_tests} ) {

		next if $test->{skip} ;

		$test->{input_sets} = [generate_data( $test, $default_sizes )] ;

		make_test_sorters( $test, $default_styles ) ;

		run_benchmarks( $test, $duration ) ;
	}
}

sub run_benchmarks {

	my( $test, $duration ) = @_ ;

	my( %entries, @input, $in_ref ) ;

	while( my( $name, $sorter ) = each %{$test->{sorters}} ) {

		$entries{ $name } = $name =~ /ref_in/ ?
			sub { my @sorted = $sorter->( $in_ref ) } :
			sub { my @sorted = $sorter->( @input ) } ;
	}

	$entries{ 'gold' } =
		sub { my @sorted = sort { $test->{gold}->() } @input } ;

	foreach my $input_set ( @{$test->{input_sets}} ) {

		my $size = @{$input_set} ;

		print "Sorting $size elements of '$test->{name}'\n" ;

		@input = @{$input_set} ;
		$in_ref = $input_set ;

		timethese( $duration, \%entries ) ;
	}
}

sub generate_data {

	my( $test, $default_sizes ) = @_ ;

	my $gen_code = $test->{gen} ;
	die "no 'gen' code for test $test->{name}" unless $gen_code ;

	my @data_sets ;

	foreach my $size ( @{ $test->{sizes} || $default_sizes || [100] } ) {

		push( @data_sets, [ map $gen_code->(), 1 .. $size ] ) ;
	}

#print Dumper \@data_sets ;

	return @data_sets if wantarray ;
	return $data_sets[0] ;
}

sub make_test_sorters {

	my( $test, $default_styles ) = @_ ;

	my $styles = $test->{styles} || $default_styles ;

#print "@{$styles}\n" ;

	my $suffix = ( $test->{ref_in} ? '_RI' : '' ) .
		     ( $test->{ref_out} ? '_RO' : '' ) ;

	my $args = $test->{args} or die "$test->{name} has no args\n" ;
	my $arg_sets = ( ref $args eq 'HASH' ) ? $args : { '' => $args } ;

	foreach my $arg_name ( sort keys %{$arg_sets} ) {

		my $test_args = $arg_sets->{$arg_name} ;

		foreach my $style ( @{$styles} ) {

			my $sort_name = $arg_name ?
				"${style}_$arg_name" : $style . $suffix ;

			my $sorter = make_sorter( $style, @{$test_args} ) ;

			die "$@\n" unless $sorter ;

			print "$sort_name source is:\n",
				sorter_source( $sorter ) if $test->{source} ;

			$test->{sorters}{$sort_name} = $sorter ;
		}
	}
}

sub count_tests {

	my( $tests, $default_styles ) = @_ ;

	my $sum = 0 ;

	foreach my $test ( @{$tests} ) {

		my $style_count = @{ $test->{styles} || $default_styles };

		my $arg_sets_count = ref $test->{args} eq 'ARRAY' ?
			1 : keys %{$test->{args}} ;

		my $test_count = $style_count * $arg_sets_count ;
		$test->{count} = $test_count ;

		$sum += $test_count ;
	}

	return $sum ;
}

my @alpha_digit = ( 'a' .. 'z', 'A' .. 'Z', '0' .. '9' ) ;
my @alpha = ( 'a' .. 'z', 'A' .. 'Z' ) ;
my @bytes = ( "\x00" .. "\xff" ) ;

sub rand_token {

	rand_string( \@alpha_digit, @_ ) ;
}

sub rand_alpha {

	rand_string( \@alpha, @_ ) ;
}

sub rand_bytes {

	rand_string( \@bytes, @_ ) ;
}

sub rand_string {

	my( $char_set, $min_len, $max_len ) = @_ ;

	$min_len ||= 8 ;
	$max_len ||= $min_len ;

	my $length = $min_len + int rand( $max_len - $min_len + 1 ) ;

	return join '', map $char_set->[rand @{$char_set}], 1 .. $length ;
}

sub rand_number {

	my( $lo_range, $hi_range ) = @_ ;

	( $lo_range, $hi_range ) = ( 0, $lo_range ) unless $hi_range ;

	my $range = $hi_range - $lo_range ;

	return rand( $range ) + $lo_range ;
}

sub rand_choice {

	my( @choices ) = @_ ;

	return @choices[rand @choices] ;
}

1 ;
