package Sort::Maker;

use base qw(Exporter);

use Data::Dumper ;

@EXPORT = qw( make_sorter );
%EXPORT_TAGS = ( 'all' => [ qw( sorter_source ), @EXPORT ] );
@EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

$VERSION = '0.01';

use strict;

use constant BIG_ENDIAN => pack('N', 1) eq pack('L', 1) ;
use constant FLOAT_LEN => length pack "d", 1 ;
use constant INT_LEN => length pack "N", 1 ;
use constant INT_BIT_LEN => INT_LEN * 8 ;
use constant FLOAT_PACK => BIG_ENDIAN ?
		q{pack( 'd', $val )} :
		q{reverse( pack( 'd', $val ) )} ;

my @boolean_attrs = qw(
	ascending
	descending
	case
	no_case
	signed
	unsigned
	signed_float
	unsigned_float
	varying
) ;

my @value_attrs = qw(
	fixed
) ;

my @grt_num_attrs = qw(
	signed
	unsigned
	signed_float
	unsigned_float
) ;

my @grt_string_attrs = qw(
	varying
	fixed
) ;

# these key attributes set are mutually exclusive
# only one can be set in the defaults or in any given key

my @mutex_attrs = (
	[qw(case no_case)],
	[qw(ascending descending)],
	\@grt_num_attrs,
	\@grt_string_attrs,
) ;


# code can only be an attribute and not a default attribute argument

my %is_boolean_attr = map { $_ => 1 } @boolean_attrs ;
my %is_value_attr = map { $_ => 1 } @value_attrs, 'code' ;

my @boolean_args = qw(
	ref_in
	ref_out
	string_data
) ;

my @value_args = qw(
	name
	init_code
) ;

# all the attributes can be set with defaults

my %is_boolean_arg = map { $_ => 1 } @boolean_args, @boolean_attrs ;
my %is_value_arg = map { $_ => 1 } @value_args, @value_attrs ;

my @key_types = qw(
	string
	number
) ;

my %is_key_arg = map { $_ => 1 } @key_types ;

my %sort_makers = (

	plain	=> \&_make_plain_sort,
	orcish	=> \&_make_orcish_sort,
	ST	=> \&_make_ST_sort,
	GRT	=> \&_make_GRT_sort,
) ;

my %is_arg = ( %is_key_arg, %sort_makers, %is_value_arg, %is_boolean_arg ) ;

my %sources ;

# this is a file lexical so the WARN handler sub can see it.

my $eval_warnings = '' ;


sub make_sorter {

# process @_ without copying it

	my( $options, $keys ) = &_process_arguments ;

	return unless $keys ;

#print Dumper $options, $keys ;

	my $sort_maker = $sort_makers{ $options->{style} } ;

	my $source = $sort_maker->( $options, $keys ) ;

	return unless $source ;

	my $sorter ;

	{
		local( $SIG{__WARN__} ) = sub { $eval_warnings .= $_[0] } ;
		$sorter = eval $source ;
	}

	$sources{ $sorter || '' } = $source ;

	$@ = <<ERR, return unless $sorter ;

sort_maker: Can't compile this source for style $options->{style}.
Check the key extraction code for errors.

$source
$eval_warnings
$@
ERR

	if ( my $name = $options->{name} ) {

		no strict 'refs' ;

		my $package = (caller())[0] ;

		*${"${package}::$name"} = $sorter ;
	}

	return $sorter ;
}

sub _process_arguments {

	my( %options, @keys ) ;

	while( @_ ) {

		my $opt = shift ;

#print "OPT $opt\n" ;

		if ( $sort_makers{ $opt } ) {

#print "style\n" ;
	
			$@ = 
		"make_sorter: Style was already set to '$options{ style }'",
				return if $options{ style } ;

# handle optional boolean => 1
			shift if @_ && $_[0] eq '1' ;
			$options{ style } = $opt ;
			$options{ $opt } = 1 ;

			next ;
		}

		if ( $is_boolean_arg{ $opt } ) {

# handle optional boolean => 1
			shift if @_ && $_[0] eq '1' ;
			$options{ $opt } = 1 ;
			next ;
		}

		if ( $is_value_arg{ $opt } ) {

			$@ = "make_sorter: No value for argument '$opt'\n",
				return unless @_ ;

			$options{ $opt } = shift ;
			next ;
		}

		if ( $is_key_arg{ $opt } ) {

			my $key_desc = $_[0] ;

# if we have no key value or it is an option, we just have a single key. 

			if ( !defined( $key_desc ) || $is_arg{ $key_desc } ) {

				push( @keys, {
					type	=> $opt,
					}
				) ;

				next ;
			}

# if we have a hash value, it is the description for this key

			if( ref $key_desc eq 'HASH' ) {

				shift ;
				$key_desc->{type} = $opt ;
				push( @keys, $key_desc ) ;
				next ;
			}

			if( ref $key_desc eq 'ARRAY' ) {

				my $desc = _process_array_attrs(@{$key_desc}) ;
				return unless $desc ;

				$desc->{type} = $opt ;

				push( @keys, $key_desc ) ;
				next ;
			}

# not a hash ref or an option/key so it must be code for the key

			shift ;
			push( @keys, {
				type	=> $opt,
				code	=> $key_desc,
				}
			) ;
			next ;
		}

		$@ = "make_sorter: Unknown option or key '$opt'\n" ;
		return ;
	}

	$@ = "make_sorter: No keys specified", return
		unless @keys ;	
	$@ = "make_sorter: No sort style selected", return
		unless $options{style} ;

	return unless _process_defaults( \%options, \@keys ) ;

	return( \%options, \@keys ) ;
}

sub _process_defaults {

	my( $opts, $keys ) = @_ ;

	return if _has_mutex_attrs( $opts, 'defaults have' ) ;

	$opts->{init_code} ||= '' ;

	foreach my $key ( @{$keys} ) {

		return if _has_mutex_attrs( $key, 'key has' ) ;

# default extract code is $_

		$key->{'code'} ||= '$_' ;

		if( ref $key->{'code'} eq 'Regexp' ) {

			$key->{'code'} = "m($key->{'code'})" ;
		}

# set descending if it is not ascending and the default is descending.

		$key->{'descending'} ||=
			!$key->{'ascending'} && $opts->{'descending'} ;

# set no_case if it is not case and the default is no_case.

		$key->{'no_case'} ||=
			!$key->{'case'} && $opts->{'no_case'} ;

# handle GRT default attrs, both number and string
# don't use the default if an attribute is set in the key

		unless( grep( $key->{$_}, @grt_num_attrs ) ) {

			@{$key}{@grt_num_attrs} = @{$opts}{@grt_num_attrs} ;
		}

		unless( grep( $key->{$_}, @grt_string_attrs ) ) {

			@{$key}{@grt_string_attrs} =
				@{$opts}{@grt_string_attrs} ;
		}
	}

	return 1 ;
}

# this is used to check for any mutually exclusive attribute in
# defaults or keys

sub _has_mutex_attrs {

	my( $href, $name ) = @_ ;

	foreach my $mutex ( @mutex_attrs ) {

		my @bad_attrs = grep $href->{$_}, @{$mutex} ;

		next if @bad_attrs <= 1 ;

		$@ = "make_sorter: Key attribute conflict: '$name @bad_attrs'";
		return 1 ;
	}

	return ;
}

sub _process_array_attrs {

	my( @attrs ) = @_ ;

	my $desc ;

	while( @attrs ) {

		my $attr = shift @attrs ;

#print "ATTR $attr\n" ;

		if ( $is_boolean_attr{ $attr } ) {

# handle boolean => 1
			shift @attrs if $attrs[0] eq '1' ;
			$desc->{ $attr } = 1 ;
			next ;
		}

		if ( $is_value_attr{ $attr } ) {

			$@ = "make_sorter: No value for attribute '$attr'",
				return unless @_ ;

			$desc->{ $attr } = shift ;
			next ;
		}

		$@ = "make_sorter: Unknown attribute '$attr'" ;
		return ;
	}

	return( $desc ) ;
}

sub _make_plain_sort {

	my( $options, $keys ) = @_ ;

	my( @plain_compares ) ;

	foreach my $key ( @{$keys} ) {

		my $plain_compare = <<CMP ;
	do{ my( \$left, \$right ) = map { EXTRACT } \$a, \$b;
		uc \$left cmp uc \$right }
CMP

		$plain_compare =~ s/\$a, \$b/\$b, \$a/ if $key->{descending} ;
		$plain_compare =~ s/cmp/<=>/ if $key->{type} eq 'number' ;
		$plain_compare =~ s/uc //g
			unless $key->{type} eq 'string' && $key->{no_case} ;
		$plain_compare =~ s/EXTRACT/$key->{code}/ ;


		push( @plain_compares, $plain_compare ) ;
	}

# build the full compare block

	my $compare_source = join "\t\t||\n", @plain_compares ;

# handle the in/out as ref options

	my $input = $options->{ref_in} ? '@{$_[0]}' : '@_' ;
	my( $open_bracket, $close_bracket ) = $options->{ref_out} ?
		qw( [ ] ) : ( '', '' ) ;

	my $source = <<SUB ;
sub {
	$options->{init_code}
	$open_bracket
	sort {
$compare_source
	} $input $close_bracket ;
}
SUB

	return $source ;
}

sub _make_orcish_sort {

	my( $options, $keys ) = @_ ;

	my( @orcish_compares ) ;

	my $orc_ind = '1' ;

	foreach my $key ( @{$keys} ) {

		my( $l, $r ) = $key->{descending} ? qw( $b $a ) : qw( $a $b ) ;

		my $orcish_compare = <<CMP ;
	(
	  ( \$or_cache$orc_ind\{$l} ||=
		do{ my (\$val) = map { EXTRACT } $l ; uc \$val } )
			cmp
	  ( \$or_cache$orc_ind\{$r} ||=
		do{ my (\$val) = map { EXTRACT } $r ; uc \$val } )
	)
CMP

		$orc_ind++ ;

# 		$orcish_compare =~ s/\$([ab])/$1 eq 'a' ? 'b' : 'a'/ge
# 			if $key->{descending} ;
		$orcish_compare =~ s/cmp/<=>/ if $key->{type} eq 'number' ;
		$orcish_compare =~ s/uc //g
			unless $key->{type} eq 'string' && $key->{no_case} ;

		$orcish_compare =~ s/EXTRACT/$key->{code}/g ;

		push( @orcish_compares, $orcish_compare ) ;
	}

# build the full compare block

	my $compare_source = join "\t\t||\n", @orcish_compares ;

# handle the in/out as ref options

	my $input = $options->{ref_in} ? '@{$_[0]}' : '@_' ;
	my( $open_bracket, $close_bracket ) = $options->{ref_out} ?
		qw( [ ] ) : ( '', '' ) ;

	my $cache_dcl = join( ',', map "%or_cache$_", 1 .. @{$keys} ) ;

	my $source = <<SUB ;
sub {
	my ( $cache_dcl ) ;
	$options->{init_code}

	$open_bracket
	sort {
$compare_source
	} $input $close_bracket ;
}
SUB

	return $source ;
}

sub _make_ST_sort {

	my( $options, $keys ) = @_ ;

	my( @st_compares, @st_extracts ) ;
	my $st_ind = '1' ;

	foreach my $key ( @{$keys} ) {

#print Dumper $key ;

		my $st_compare = <<CMP ;
	\$a->[$st_ind] cmp \$b->[$st_ind]
CMP

		$st_compare =~ tr/ab/ba/ if $key->{descending} ;
		$st_compare =~ s/cmp/<=>/ if $key->{type} eq 'number' ;

		$st_ind++ ;

		push( @st_compares, $st_compare ) ;

		my $st_extract = <<EXT ;
		do{ my (\$val) = EXTRACT ; uc \$val }
EXT

		$st_extract =~ s/EXTRACT/$key->{code}/ ;
		$st_extract =~ s/uc //
			unless $key->{type} eq 'string' && $key->{no_case} ;

		chomp( $st_extract ) ;
		push( @st_extracts, $st_extract ) ;
	}

# build the full compare block

	my $compare_source = join "\t\t||\n", @st_compares ;

# build the full code for the key extracts

	my $extract_source = join ",\n", @st_extracts ;

# handle the in/out as ref options

	my $input = $options->{ref_in} ? '@{$_[0]}' : '@_' ;
	my( $open_bracket, $close_bracket ) = $options->{ref_out} ?
		qw( [ ] ) : ( '', '' ) ;

	my $source = <<SUB ;
sub {
	$options->{init_code}
	return $open_bracket
	map \$_->[0],
	sort {
$compare_source
	}
	map [ \$_,
$extract_source
	], $input $close_bracket ;
}
SUB

}

sub _make_GRT_sort {

	my( $options, $keys ) = @_ ;

	my( $pack_format, @grt_extracts ) ;

	my $init_code = $options->{init_code} ;

# use this to count keys so we can generate init_code for each key

	my $key_ind = '0' ;

	foreach my $key ( @{$keys} ) {

#print Dumper $key ;

		my( $key_pack_format, $grt_extract, $key_init_code ) =
			$key->{type} eq 'number' ?
				_make_GRT_number_key( $key ) :
				_make_GRT_string_key( $key, $key_ind++ ) ;

#print "[$key_pack_format] [$grt_extract] [$key_init_code]\n" ;

		return unless $key_pack_format ;

		$pack_format .= $key_pack_format ;
		$init_code .= $key_init_code || '' ;

		chomp( $grt_extract ) ;
		push( @grt_extracts, $grt_extract ) ;
	}

############
# pack the record index.
# SKIP for 'string_data' attribute
##########

	$pack_format .= 'N' ;

	my $extract_source = join ",\n", @grt_extracts ;
	chomp( $extract_source ) ;

# handle the in/out as ref options

	my $input = $options->{ref_in} ? '@{$_[0]}' : '@_' ;
	my( $open_bracket, $close_bracket ) = $options->{ref_out} ?
		qw( [ ] ) : ( '', '' ) ;


	my $get_index_code = <<INDEX ;
unpack( 'N', substr( \$_, -INT_LEN ) )
INDEX
	chomp $get_index_code ;

	my $source = $options->{string_data} ? <<STRING_DATA : <<REF_DATA ;
sub {
	my \$rec_ind = 0 ;
$init_code
	return $open_bracket ${input}\[
	    map $get_index_code, 
	    sort
	    map pack( "$pack_format",
$extract_source,
		\$rec_ind++
	    ), ${input}
        ] $close_bracket;
}
STRING_DATA
sub {
	my \$rec_ind = 0 ;
$init_code
	return $open_bracket ${input}\[
	    map $get_index_code, 
	    sort
	    map pack( "$pack_format",
$extract_source,
		\$rec_ind++
	    ), ${input}
        ] $close_bracket;
}
REF_DATA

	$source =~ s/INT_LEN/INT_LEN/ge ;
	$source =~ s/FLOAT_LEN/FLOAT_LEN/ge ;
	$source =~ s/FLOAT_PACK/FLOAT_PACK/ge ;

#print $source ;

	return $source ;
}


sub _make_GRT_number_key {

	my( $key ) = @_ ;

	my( $pack_format, $val_code, $negate ) ;

	if ( $key->{descending} ) {

		$negate = '-' ;

		$key->{signed} = 1 if delete $key->{unsigned} ;
		$key->{signed_float} = 1 if delete $key->{unsigned_float} ;
	}
	else {

		$negate = '' ;
	}

	if ( $key->{unsigned} ) {

		$pack_format = 'N' ;
		$val_code = '$val' ;
	}
	elsif ( $key->{signed} ) {

		$pack_format = 'N' ;
		$val_code = '$val ^ (1 << (INT_BIT_LEN - 1))'
	}
	elsif ( $key->{unsigned_float} ) {

		$pack_format = 'AFLOAT_LEN' ;
		$val_code = FLOAT_PACK ;
	}
	else {

# must be a signed float

		my $xor_neg = '\xFF' x FLOAT_LEN ;
		$pack_format = 'AFLOAT_LEN' ;
		$val_code = qq{
			( \$val < 0 ? "$xor_neg" : "\\x80" ) ^ FLOAT_PACK } ;
	}

	my $grt_extract = <<CODE ;
		do{ my (\$val) = $key->{code} ; $val_code }
CODE

	return( $pack_format, $grt_extract, '' ) ;
}

sub _make_GRT_string_key {

	my( $key, $key_ind ) = @_ ;

	my( $init_code, $pack_format ) ;

	if ( my $fix_len = $key->{fixed} ) {

# create the xor string to invert the key for a descending sort.
		$init_code = <<CODE if $key->{descending} ;
	my \$_xor$key_ind = "\\xFF" x $fix_len ;
CODE
		$pack_format = "a$fix_len" ;

	}
	elsif ( $key->{varying} ) {

# create the code to scan for the maximum length of the values for this key

		$init_code = <<CODE ;
	use List::Util qw( max ) ;
	my \$len$key_ind = max(
		map { my (\$val) = $key->{code} ; length \$val } \@_
	) ;
CODE

#  create the xor string to invert the key for a descending sort.

		$init_code .= <<CODE if $key->{descending} ;
	my \$_xor$key_ind = "\\xFF" x \$len$key_ind ;
CODE

# we pack as a null padded string. its length is in the 

		$pack_format = "a\${len$key_ind}" ;
	}
	else {

# we can't sort plain (null terminated) strings in descending order 

		$@ = <<ERR, return if $key->{descending} ;
make_sorter: A GRT descending string needs to select either the
'fixed' or 'varying' attributes
ERR

		$pack_format = 'Z*' ;
	}

	my $descend_code = $key->{descending} ? " ^ \$_xor$key_ind" : '' ;

	my $grt_extract = <<CODE ;
		do{ my( \$val ) = EXTRACT ; uc( \$val )$descend_code }
CODE

	$grt_extract =~ s/EXTRACT/$key->{code}/ ;
	$grt_extract =~ s/uc// unless $key->{no_case} ;

	return( $pack_format, $grt_extract, $init_code ) ;
}

sub sorter_source {

	$sources{ +shift || '' } ;
}

1 ;

__END__

=head1 NAME

Sort::Maker - A simple way to make efficient sort subs

=head1 SYNOPSIS

	use Sort::Maker ;

	my $sorter = make_sorter( ... ) ;


=head1 DESCRIPTION

This module has two main goals: to make it easy to create correct sort
functions, and to make it simple to select the optimum sorting
algorithm for the number of items to be sorted. Sort::Maker generates
complete sort subroutines in one of four styles, plain, orcish
manouver, Schwartzian Transform and the Guttman-Rosler Transform. You
can also get the source for a sort sub you create via the
sorter_source call.

=head1 C<make_sorter>

The sub C<make_sorter> is exported by Sort::Maker. It makes a sort sub
and returns a reference to it. You describe how you want it to sort
its input by passing general options and key descriptions to
C<make_sorter>.

=head2 Arguments to C<make_sorter>

There are two types of arguments, boolean and value. Boolean arguments
can be set just with the option name and can optionally be followed by
'1'. You can easily set multiple boolean general arguments with
qw(). Value arguments must have a following value.  Arguments can
appear in any order but the key descriptions (see below) must appear
in their sort order. The code examples below show various ways to set
the various arguments.

Arguments fall into four categories: selecting the style of the sort,
key descriptions, setting defaults for key description attributes, and
setting general flags and values. The following sections will describe
the categories and their associated arguments.

=head2 Sort Style

The style of the sort to be made is selected by setting one of the
following Boolean arguments. Only one must be set otherwise an error
is reported (see below for error handling). Also see below for
detailed descriptions of the supported sort styles.

	plain
	orcish
	ST
	GRT

	# Make a plain sorter
	my $plain_sorter = make_sorter( qw( plain ) ... ) ;

	# Make an orcish manouevre sorter
	my $orcish_sorter = make_sorter( orcish => 1 ... ) ;

	# Make a Schwartzian Transform sorter
	my $st_sorter = make_sorter( 'ST', 1, ... ) ;

	# Make a GRT sort
	my $GRT = make_sorter( 'GRT', ... ) ;

=head2 Key Attribute Defaults

The following arguments set defaults for the all of the keys'
attributes.  These default values can be overridden in any individual
key.  Only one of the attributes in each of the groups below can be
set as defaults or for any given key. If more than one attribute is
each group is set, then C<make_sorter> will return an error.  The
attribute that is the default for each group is marked.  See below for
details on key attributes.

	ascending	(default)
	descending

	no_case		(default)
	case

	signed
	unsigned
	signed_float	(default)
	unsigned_float

	fixed
	varying

=head2 General Options

These arguments set general options that apply to how the generated
sorter interacts with the outside world.

=head3 name

This is a value option which set the name of the sorter sub. The sort
sub will be created and exported to the caller's namespace with this
name.

	make_sorter( name => 'my_sorter', ... ) ;
	@sorted = my_sorter @unsorted ;

=head3 ref_in/ref_out

This boolean arguments specifies that the input to and output from the
sort sub will be array references. C<ref_in> makes the sorter only
take as input a single array reference (which contains the unsorted
records). C<ref_out> makes the sorter only return a single array
reference (which contains the sorted records). You can set both of
these options in a sorter.

Note: This does not affect key extraction code which still gets each
record in C<$_>. It only modifies the I/O of the generated sorter.

	# input records are in an array reference
	my $sorter = make_sorter( qw( ref_in ), ... ) ;
	@sorted_array = $sorter->( \@unsorted_input ) ;

	# sorted output records are in an array reference
	my $sorter = make_sorter( ref_out => 1, ... ) ;
	$sorted_array_ref = $sorter->( @unsorted_input ) ;

	# input and output records are in array references
	my $sorter = make_sorter( qw( ref_in ref_out ), ... ) ;
	$sorted_array_ref = $sorter->( \@unsorted_input ) ;

=head3 C<string_data>

This boolean argument specifies that the input records will be
strings.  It is only valid for use with the GRT and it is ignored for
the other sort styles. It tells the GRT that it can put the record
directly into the string cache. See the section below on the GRT for
more detail.

=head3 C<init_code>

This value argument is code that will be put into the beginning of the
generated sorter subroutine. It is meant to be used to declare lexical
variables that the extraction code can use. Normally different
extraction code have no way to share common code. By declaring
lexicals with the C<init_code> option, then some key extraction code
can save data there for use by another key. This is useful if you have
two (or more) keys that share a complex piece of code such as
accessing a deep value is a record tree.

For example if the input record is an array of arrays of hashes
strings and the string has 2 keys that need to be grabbed by a
regex. The string is a string key, a ':' and a number key. So the
common part of the key extraction is:

	$_->[0][0]{a}

And the make_sorter call is:

	my $sorter = make_sorter( 
		init_code => 'my( $str, $num ) ;',
		string => 'do{( $str, $num ) =
			$_->[0][0]{a} =~ /^(\w+):(\d+)$/; $str}',
		number => '$num'
	) ;

In that code both keys are extracted in the first key extraction code
and the number key is saved in C<$num>. The second key extraction code
just uses that saved value.

=head2 Key Description Arguments

Sorting data requires that records be compared in some way so they can
be put into a proper sequence. The parts of the records that actually
get compared are called its keys. In the simplest case the entire
record is the key, as when you sort a list of numbers or file
names. But in many cases the keys are embedded in the full record and
they need to be extracted before they can be used in comparisons.
Sort::Maker uses key descriptions that denote the from the record, and
optional other attributes that will help optimize the sorting
operation. This section will explain how to pass key description
arguments to the make_sorter subroutine and what the various
attributes mean and how to best use them.

The generated sorter will sort the records according to the order of
the key arguments. The first key is used to compare a pair records and
if are deemed equal, then the next key is examined. This happens until
the records are given an ordering or you run out of keys and the
records are deemed equal in sort order.  Key descriptions can be mixed
with the other arguments which can appear in any order and anywhere in
the argument list, but the keys themselves must be in the desired
order.

A key argument is either 'string' or 'number' followed by optional
attributes. The key type sets the way that the key is compared
(e.g. using 'cmp' or '<=>').  All key attributes can be set from the
default values in the global arguments or set in each individual key
description.

There are 4 ways to provide attributes to a key:

=head3 No attributes

A key argument which is either at end of the argument list or is
followed by a valid keyword token has no explict attributes. This key
will use the default attributes.  In both of these examples, a default
attribute was set and used by the key description which is just a
single key argument.

	# sort the record as a single number in descending order
	my $sorter = make_sorter( qw( plain number descending ) ) ;

	# sort the record as a case sensitive string
	my $sorter = make_sorter( qw( plain case string ) ) ;

	# sort the record as a single number in ascending order
	my $sorter = make_sorter( qw( ST number ) ) ;

=head3 Only Code as a Value

A key argument which is followed by a scalar value which is not a
valid keyword token, will use that scalar value as its key extraction
code. See below for more on key extraction code.

	# sort by the first (optionally signed) number matched
	my $sorter = make_sorter( qw( plain number /([+-]?\d+)/ ) ) ;

	# string sort by the 3rd field in the input records (array refs)
	my $sorter = make_sorter( 'ST', string => '$_->[2]' ) ;

=head3 An Array Reference

A key argument which is followed by an array reference will parse that
array for its description attributes. As with the general boolean
arguments, any boolean attribute can be optionally followed by a
'1'. Value attributes must be followed by their value.

	# another way to specify the same sort as above
	# sort by the first (optionally signed) number matched

	my $sorter = make_sorter(
		qw( plain ),
		number => [
			code => '/(\d+)/',
			'descending',
		],
	) ;

	# same sort but for the GRT which uses the 'unsigned'
	# attribute to optimize the sort.

	my $sorter = make_sorter(
		qw( GRT ),
		number => [
			qw( descending unsigned ),
			code => '/(\d+)/',
		],
	) ;

=head3 A Hash Reference

A key argument which is followed a hash reference will use that hash
as its description attributes. Any boolean attribute in the hash must
have a value of '1'.  Value attributes must be followed by their
value.

	# another way to specify the same sort as above
	# sort by the first (optionally signed) number matched

	my $sorter = make_sorter(
		qw( plain ),
		number => {
			code => '/(\d+)/',
			descending => 1,
		],
	) ;

	# a multi-key sort. the first key is a descending unsigned
	# integer and the second is a string padded to 10 characters

	my $sorter = make_sorter(
		qw( GRT ),
		number => {
			code => '/(\d+)/',
			descending => 1,
			unsigned => 1,
		},
		string => {
			code => '/FOO<(\w+)>/',
			fixed => 10,
		},
	) ;

=head2 Key Description Attributes

What follows are the attributes for key descriptions. Most use 
the default values passes in the arguments to C<make_sorter>.

=head3 code

This value attribute is the code that will be used to extract a key
from the input record. It must be a string of Perl code that operates
on $_ and extracts a value.  The code will be wrapped in a do{} block
and called in a list context so that regular expressions can just use
() to grab a key value. The code defaults to C<$_> which means the
entire record is used for this key. You can't set the default for code
(unlike all the other key attributes). See the section on Extraction
Code for more.

	# make an ST sort of the first number grabbed in descending order

	my $sorter = make_sorter(
		qw( ST ),
		number => {
			code	=> '/(\d+)/',
			descending => 1,
		},
	) ;

=head3 ascending/descending

These two attributes control the sorting order for this key. If a key
is marked as C<ascending> (which is the initial default for all keys),
then lower keys will sort before higher keys. C<descending> sorts have
the higher keys sort before the lower keys. It is illegal to have both
set in the defaults or in any key.

	# sort by descending order of the first grabbed number
	# and then sort in ascending order the first grabbed <word>

	my $sorter = make_sorter(
		qw( ST descending ),
		number => {
			code	=> '/(\d+)/',
		},
		number => {
			code	=> '/<(\w+)>/',
			ascending => 1,
		},
	) ;

	# this will return undef and store an error in $@. 
	# you can't have both 'ascending' and 'descending' as defaults

	my $sorter = make_sorter(
		qw( ST ascending descending ),
		number => {
			code	=> '/(\d+)/',
			descending => 1,
		},
	) ;

	# this will return undef and store an error in $@. 
	# you can't have both 'ascending' and 'descending' in a key

	my $sorter = make_sorter(
		qw( ST )
		number => {
			code	=> '/(\d+)/',
			descending => 1,
			ascending => 1,
		},
	) ;

=head3 case/no_case

These two attributes control the how 'string' keys handle case
sensitivity. If a key is marked as C<case> (which is the initial
default for all keys), then keys will treat upper and lower case
letters as different.  If the key is marked as C<no_case> then they
are treated as equal.  It is illegal to have both set in the defaults
or in any key. Internally this uses the uc() function so you can use
locale settings to affect string sorts.

	# sort by the first grabbed word with no case
	# and then sort the grabbed <word> with case

	my $sorter = make_sorter(
		qw( ST no_case ),
		string => {
			code	=> '/(\w+)/',
		},
		string => {
			code	=> '/<(\w+)>/',
			case => 1,
		},
	) ;

	# this will return undef and store an error in $@. 
	# you can't have both 'case' and 'no_case' as defaults

	my $sorter = make_sorter(
		qw( ST no_case case ),
		string => {
			code	=> '/(\w+)/',
		},
	) ;

	my $sorter = make_sorter(
		qw( ST ascending descending ),
		number => {
			code	=> '/(\d+)/',
			descending => 1,
		},
	) ;

	# this will return undef and store an error in $@. 
	# you can't have both 'case' and 'no_case' in a key

	my $sorter = make_sorter(
		qw( ST )
		string => {
			code	=> '/(\w+)/',
			no_case	=> 1,
			case	=> 1,
		},
	) ;

=head3 signed/unsigned/signed_float/unsigned_float (GRT only)

These attributes are only used by the GRT sort style. They are meant
to describe the type of a number key so that the GRT can best process
and cache the key's value. It is illegal to have more than one of them
set in the defaults or in any key. See the section on GRT sorting for
more.

The C<signed> and C<unsigned> attributes mark this number key as an
integer. The GRT does the least amount of work processing an unsigned
integer and only slightly more work for a signed integer. It is worth
using these attributes if a sort key is restricted to integers.

The C<signed_float> (which is the normal default for all keys) and
C<unsigned_float> attributes mark this number key as a float. The GRT
does the less work processing an unsigned float then a signed float.
It is worth using the C<unsigned_float> attribute if a sort key is
restricted to non-negative values. The C<signed_float> attribute is
supported to allow overriding defaults and to make it easier to
auto-generate sorts.

=head3 fixed/varying (GRT only)

These attributes are only used by the GRT sort style. They are meant
to describe the type of a string key so that the GRT can properly
process and cache the key's value. It is illegal to have more than one
of them set in the defaults or in any key. See the section on GRT
sorting for more.

C<fixed> is a Boolean attribute that marks this string key as always
being a fixed length. The extracted value will either be padded with
null (0x0) bytes or truncated to the specified length. The data in
this key can have embedded null bytes (0x0) and also it can be sorted
in descending order.

C<varying> is a Boolean attribute marks this string key as being of
varying lengths. The GRT sorter will do a scan of all of this key's
values to find the maximum string length and then it pads all the
extracted values to that length. The data in this key can have
embedded null bytes (0x0) and also it can be sorted in descending
order. 

=head2 Key Extraction Code

Each input record must have its sort keys extracted from the data. This
is the purpose of the 'code' attribute in key descriptions. The code
must be a single string which operates on a record which is in C<$> and
it must return the key value. The code is executed in a list context so
you can use grabs in m// to return the key. Note that only the first
grab will be used but you shouldn't have more than one anyway. See the
examples below.

Code can be either a string or a qr// (Regexp) object. If qr// is
used, the actual generated code will be m($qr) which works because
qr// will interpolate to its string representation. The advantage of
qr// over a string is that the qr// will be syntax checked at compile
time while the string only later when the generated sorter is compiled
by an eval.

Extraction code for a key can be set in one of three ways.

=head3 No explicit code

If you don't pass any extraction code to a key, it will default to C<$_>
which is the entire record. This is useful in certain cases such as in
simple sorts where you are sorting the entire record.

	# sort numerically and in reverse order
	my $sorter = make_sorter( qw( plain number descending ) ;

	# sort with case folding
	my $sorter = make_sorter( qw( plain no_case string ) ;

	# sort by file time stamp and then by name
	my $sorter = make_sorter( 'ST', number => -M, 'string' ) ;

=head3 Code is the only key attribute

In many cases you don't need to specify any specific key attributes (the
normal or globally set defaults are fine) but you need extraction
code. If the argument that follows a key type ( 'string' or 'number' )
is not a valid keyword, it will be assumed to be the extraction code
for that key.

	# grab the first number string as the key
	my $sorter = make_sorter( qw( plain number /(\d+)/ ) ) ;

	# no_case string sort on the 3rd-5th chars of the 2nd array element
	my $sorter = make_sorter(
                plain	=> 1,
                no_case => 1,
                string	=> 'substr( $_->[1], 2, 3)'
	) ;

=head3 Key needs specific attributes

When the key needs to have its own specific attributes other than its
code, you need to pass them in an ARRAY or HASH reference. This is
mostly needed when there are multiple keys and the defaults are not
correct for all the keys.

	# string sort by the first 3 elements of the array record with
        # different case requirements
	
	my $sorter = make_sorter(
                ST	=> 1,
                string	=> {
                        code	=> '$_->[0]',
                        no_case => 1,
                },
                string	=> '$_->[1]',
                string	=> {
                        code	=> '$_->[2]',
                        no_case => 1,
                },
	) ;

	# GRT sort with multiple unsigned integers and padded strings
	# note that some keys use a hash ref and some an array ref
	# the record is marked with key\d: sections
	my $sorter = make_sorter(
                GRT	=> 1,
                descending => 1,
                number	=> {
                        code	=> 'key1:(\d+)',
                        unsigned => 1,
                },
                number	=> [
                        code	=> 'key2:([+-]?\d+)',
                        qw( signed ascending ),
                ],
                string	=> [
                        code	=> 'key3:(\w{10})',
                        fixed => 1,
                        ascending => 1,
                ],
		# pad the extracted keys to 8 chars
                string	=> {
                        code	=> 'key4:([A-Z]+)',
                        pad => 8,
                },
	) ;

=head1 Key Caching

A good question to ask is "What speed advantages do you get from this
module when all the sorts generated use Perl's internal sort function?"
The sort function has a O( N * log N ) growth function which means that
the amount of work done increases by that formula as N (the number of
input records) increases. In a plain sort this means the the key
extraction code is executed N * log N times when you only have N
records. That can be a very large waste of cpu time. So the other three
sort styles speed up the overall sort by only executing the extraction
code N times by caching the extracted keys. How they cache the keys is
their primary difference. To compare or study the actual code generated
for the different sort styles, you can run make_sorter and just change
the style. Then call sorter_source (not exported by default) and pass it
the sort code reference returned by make_sorter. It will return the
generated sort source.

=head2 plain

Plain sorting doesn't do any key caching. It is fine for short input
lists (see the Benchmark section) and also as a way to see how much CPU
is saved when using one of the other styles.

=head2 orcish

The Orcish maneuvre (created by Joseph Hall) caches the extracted keys
in a hash. It does this with code like this:

	$cache{$a} ||= CODE($a) ;

CODE is the extract code and it operates on a record in $a. If we have
never seen this record before then the cache entry will be undef and the
||= operator will assign the extracted key to that hash slot. The next
time this record is seen in a comparison, the saved extracted key will
be found in the hash and used. The name orcish comes from OR-cache.

=head2 ST

The ST (Schwartzian Transform and popularized by Randal Schwartz) uses
an anonymous array to store the record and its extracted keys. It first
execute a map that creates an anonymous array:

	map [ $_, CODE1( $_ ), CODE2( $_ ) ], @input

The CODE's extract the set of keys from the record but only once per
record so it is O(N). Now the sort function can just do the comparisons
and it returns a list of sorted anonymous arrays.

	sort {
		$a->[1] cmp $b->[1]
			||
		$a->[2] cmp $b->[2]
	}

Finally, we need to get back the original records which are in the first
slot of the anonymous array:

	map $_->[0]

This is why the ST is known as a map/sort/map technique.

=head2 GRT

The Guttman-Rosler Transform (popularized by Uri Guttman and Larry
Rosler) uses a string to cache the extracted keys as well as either
the record or its index. It is also a map/sort/map technique but
because its cache is a string, it can be sorted without any Perl level
callback (the {} block passed to sort). This is a signifigant win
since that callback is running O( N log N). But this speedup comes at
a cost of complexity. You can't just join the keys into a string and
properly sort them. Each key may need to be processed so that it will
correctly sort in order and it doesn't interfere with other keys. That
is why the GRT has several key attributes to enable it to properly and
efficiently pack the sort keys into a single string. The following
lists the GRT key attributes, when you need them and what key
processing is done for each.  Note that you can always enable the GRT
specific attributes as they are just ignored by the other sort styles.

=head2 GRT

The GRT gains its speed by using a single byte string to cache all of
the extracted keys from a given input record. Packing keys into a
string such that it will lexically sort the correct way requires some
deep mojo and data munging. But that is why this module was written -
to hide all that from the coder. Below are descriptions of how the
various key types are packed and how to best use the GRT specific key
attributes.  Note: you can only use one of the GRT number or string
attributes for any key. Setting more than one in either the defaults
or in any given key is an error (a key's attribute can override a
default choice).

=head3 C<unsigned>

The 'unsigned' attribute tells the GRT that this number key is a
non-negative integer. This allows the GRT to just pack it into 4 bytes
using the N format (network order - big endian). An integer packed this
way will have its most significant bytes compared before its least
signifigant bytes. This involves the least amount of key munging and so
it is the most efficient way to sort numbers in the GRT.

If you want this key to sort in descending order, then the key value is
negated and normalized (see the 'signed' attribute) so there is no
advantage to using 'unsigned'.

=head3 signed

The 'signed' attribute tells the GRT that this number key is an
integer. This allows the GRT to just pack it into 4 bytes using the N
format (network order - big endian).  The key value must first be
normalized which will convert it to an unsigned integer but with the
same ordering as a signed integer. This is simply done by inverting
the sign (highest order) bit of the integer. As mentioned above, when
sorting this key in descending order, the GRT just negates the key
value.

NOTE: In the GRT the signed and unsigned integer attributes only work on
perl built with 32 bit integers. This is due to using the N format of
pack with is specified to be 32 bits. A future version may support 64
bit integers (anyone want to help?).

=head3 C<unsigned_float>

The 'unsigned_float' attribute tells the GRT that this number key is a
non-negative floating point number. This allows the GRT to pack it
into 8 bytes using the 'd' format. A float packed this way will have
its most significant bytes compared before its least signifigant
bytes. 

=head3 C<signed_float>

The C<signed_float> attribute (which is the default for all number
keys when using the GRT) tells the GRT that this number key is a
floating point number. This allows the GRT to pack it into 8 bytes
using the 'd' format. A float packed this way will have its most
significant bytes compared before its least signifigant bytes. When
processed this key will be normalized to an unsigned float similar to
to the C<signed> to C<unsigned> conversion mentioned above.

NOTE: The GRT only works with floats that are in the IEEE format for
doubles. This includes most modern architectures including x86, sparc,
powerpc, mips, etc. If the cpu doesn't have IEEE floats you can either
use the integer attributes or select another sort style (all the
others have no restriction on float formats).

=head3 simple string.

If a string key is being sorted in ascending order with the GRT and it
doesn't have one of the GRT string attributes, it will be packed without
any munging and a null (0x0) byte will be appended to it. This byte
enables a shorter string to sort before longer ones that starts with the
shorter string.

NOTE: You cannot sort strings in descending order in the GRT unless
the key has either the 'fixed' or 'varying' attributes set. Also if a
string is being sorted in ascending order but has any null (0x0) bytes
in it, the key must have one of those attributes set.

=head3 C<fixed>

This boolean attribute tells the GRT to pack this key value as a fixed
length string. The extracted value will either be padded with null
(0x0) bytes or truncated to the specified length. This means it can be
packed into the cache string with no padding and no trailing null byte
is needed. The key can contain any data including null (0x0)
bytes. The only data munging is if the key's sort order is descending,
then the key value is xor'ed with a same length string of 0xff
bytes. This toggles each bit which allows for a lexical comparison but
in the reverse order. This same bit inversion is used for descending
varying strings.

=head3 C<varying>

This Boolean attribute tells the GRT that this key value is a varying length
string and has no predetermined padding length. A prescan is done to
determine the maximum string length for this key and that is used as the
padding length. The rest is the same as with the 'fixed' attribute.

=head2 C<sorter_source>

This sub (which can be exported) returns the source of a generated
sort sub or the source of the last one that had an error. To get the
source of an existing sort sub, pass it a reference to that sub (i.e.
the reference returned from make_sorter). To get the source for a
failed call to make_sorter, don't pass in any arguments.

	my $sorter = make_sorter( ... ) ;
	print sorter_source( $sorter ) ;

	make_sorter( name => 'my_sorter', ... ) ;
	print sorter_source( \&my_sorter ) ;

	my $sorter = make_sorter( ... ) 
		or die "make_sorter error: $@\n", sorter_source();

If all you want is the generated source you can just do:

	print sorter_source make_sorter( ... ) ;

=head2 Error Handling

When C<make_sorter> detects an error (either bad arguments or when the
generated sorter won't compile), it returns undef and set $@ to an
error message. The error message will include the generated source and
compiler and warning errors if the sorted didn't compile correctly.
The test t/errors.t covers all the possible error messages.  You can
also retrieve the generated source after a compiling error by calling
C<sorter_source>.

=head1 TESTS

C<Sort::Maker> uses a table of test configurations that can both run
tests and benchmarks. Each test script is mostly a table that generate
multiple versions of the sorters, generate sample data and compare the
sorter results with a sort that is known to be good. If you run the
scripts directly and with a -bench argument, then they generate the
same sorter subs and benchmark them. This design ensures that
benchmarks are running on correctly generated code and it makes it
very easy to add more test and benchmark variations. The code that
does all the work is in t/common.pl. Here is a typical test table
entry:

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

C<skip> is a boolean that causes this test/benchmark to be skipped.
Setting C<source> causes the sorter's source to be printed out.
C<gen> is a sub that generated a single record. There are random data
subs in t/common.pl. Some tests have a C<data> field which is fixed
data for a test (instead of the generated data). The <gold> field is a
comparision subroutine usable by the sort function. It is used to sort
the test data into a golden result which is used to compare against
all the generated sorters. C<args> is an anonmyous of arguments for a
sorter or a hash ref with multiple named/args pairs. See t/io.t for an
example of that.


=head1 BENCHMARKS

=head1 EXPORT

This module always exports the C<make_sorter> sub.  It can also
optionally export C<sorter_source>.

=head1 AUTHOR

Uri Guttman, E<lt>uri@stemsystems.comE<gt>

=head1 ACKNOWLEDGEMENTS

I would like to thank the inimitable Damian Conway for his help in the
API design, the POD, and for being a good Perl friend.

And thanks to Boston.pm for the idea of allowing qr// for key
extraction code.

=cut
