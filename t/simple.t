#!/usr/local/bin/perl -w

use strict ;

use lib 't' ;
use lib '..' ;
require 'common.pl' ;

my @sort_styles = qw( plain orcish ST GRT ) ;

my $sort_tests = [

	{
		skip	=> 0,
		name	=> 'simple string',
		data	=> [ qw( z e a k ) ],
		gold	=> sub { $a cmp $b },
		args	=> [ qw( string ) ],
	},
	{
		skip	=> 0,
		name	=> 'simple number',
		data	=> [ 32, 2, 9, 7 ],
		gold	=> sub { $a <=> $b },
		args	=> [ qw( number ) ],
	},
] ;

test_driver( $sort_tests, \@sort_styles ) ;

exit ;
