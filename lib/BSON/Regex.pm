package BSON::Regex;

use base qw/BSON::Types::Regex/;

use strict;
use warnings;

sub new {
    my ( $class, $pattern, $flags ) = @_;
    bless { pattern => $pattern, flags => $flags }, $class;
}

sub pattern {
    $_[0]->{pattern};
}

sub flags {
    $_[0]->{flags};
}

1;