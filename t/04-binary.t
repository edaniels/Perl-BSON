#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 8;
use BSON;

my $bin = BSON::Binary->new( [ 1, 2, 3, 4, 5 ] );
isa_ok( $bin, 'BSON::Binary' );
is_deeply( $bin->data, "\x01\x02\x03\x04\x05" );
is( $bin->subtype, 0 );
is_deeply( [ unpack 'C*', $bin->to_s ], [ 5, 0, 0, 0, 0, 1, 2, 3, 4, 5 ] );

$bin = BSON::Binary->new( "\1\2\3\4\5", 5 );
isa_ok( $bin, 'BSON::Binary' );
is_deeply( $bin->data, "\x01\x02\x03\x04\x05" );
is( $bin->subtype, 5 );
is_deeply( [ unpack 'C*', $bin->to_s ], [ 5, 0, 0, 0, 5, 1, 2, 3, 4, 5 ] );
