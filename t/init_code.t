#!/usr/local/bin/perl -w

use strict ;

use lib 't' ;
use lib '..' ;
require 'common.pl' ;

#my @sort_styles = qw( ST ) ;
my @sort_styles = qw( plain orcish ST GRT ) ;

my @string_keys = map rand_alpha( 4, 8 ), 4 ;
my @number_keys = map int rand_number( 100, 10000 ), 4 ;

my $sort_tests = [

	{
		skip	=> 0,
		source	=> 0,
		name	=> 'init_code',
		gen	=> sub { rand_choice( @string_keys ) . ':' .
				 rand_choice( @number_keys ) },
		gold	=> sub {
			 ($a =~ /^(\w+)/)[0] cmp ($b =~ /^(\w+)/)[0]
			 		||
			 ($a =~ /(\d+$)/)[0] <=> ($b =~ /(\d+$)/)[0] 
		},
		args	=> [
			init_code => 'my( $str, $num ) ;',
			string => 'do{( $str, $num ) = /^(\w+):(\d+)$/; $str}',
			number => '$num',
		],
	},
	{
		skip	=> 0,
		source	=> 0,
		name	=> 'deep init_code',
		gen	=> sub { [[{'a' => rand_choice( @string_keys ) . ':' .
				 rand_choice( @number_keys )}]] },
		gold	=> sub {
			 ($a->[0][0]{a} =~ /^(\w+)/)[0] cmp
			 ($b->[0][0]{a} =~ /^(\w+)/)[0]
			 		||
			 ($a->[0][0]{a} =~ /(\d+$)/)[0] <=>
			 ($b->[0][0]{a} =~ /(\d+$)/)[0] 
		},
		args	=> [
			init_code => 'my( $str, $num ) ;',
			string => 'do{( $str, $num ) =
				$_->[0][0]{a} =~ /^(\w+):(\d+)$/; $str}',
			number => '$num',
		],
	},
] ;

test_driver( $sort_tests, \@sort_styles ) ;

exit ;
