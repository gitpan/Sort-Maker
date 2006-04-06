#!/usr/local/bin/perl -w

use strict ;

use Test::More tests => 13 ;
use Carp ;

BEGIN{ 

	use_ok( 'Sort::Maker', ':all' ) ;
}

ok( defined \&make_sorter, 'export' ) ;

my $err ;

$err = make_sorter( qw( xxx ) ) ;
ok( !defined $err && $@ =~ /unknown/i, 'unknown option' ) ;

$err = make_sorter( ) ;
ok( !defined $err && $@ =~ /no keys/i, 'no keys' ) ;

$err = make_sorter( qw( ST GRT ) ) ;
ok( !defined $err && $@ =~ /style was already set/i, 'duplicate style' ) ;

$err = make_sorter( qw( name ) ) ;
ok( !defined $err && $@ =~ /no value/i, 'no value' ) ;

$err = make_sorter( qw( no_case string ) ) ;
ok( !defined $err && $@ =~ /no sort style/i, 'no style' ) ;

$err = make_sorter( 'plain', string => { ascending => 1, descending => 1 } ) ;
ok( !defined $err && $@ =~ /has ascending/i, 'ascending and descending' ) ;

$err = make_sorter( 'plain', string => { case => 1, no_case => 1 } ) ;
ok( !defined $err && $@ =~ /has case/i, 'case and no_case' ) ;

$err = make_sorter( 'plain', string => 'XXX' ) ;
ok( !defined $err && $@ =~ /compile/i, 'illegal code' ) ;

$err = make_sorter( qw( GRT string descending ) ) ;
ok( !defined $err && $@ =~ /descending string/i, 'GRT descending string' ) ;

$err = make_sorter(
	qw(
		ref_in
		ref_out
		ST
	),
	number => [
		qw(
			descending
			unsigned_float
		),
		'code',
	],
) ;
ok( !defined $err && $@ =~ /No value/, 'array args - no value' ) ;

$err = make_sorter(
	qw(
		ST
	),
	number => [
		qw(
			descending
			unsigned_float
		),
		'foobar',
	],
) ;
ok( !defined $err && $@ =~ /Unknown attribute/,
	'array args - unknown attribute' ) ;


