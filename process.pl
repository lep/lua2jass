#!/bin/env perl

#use v5.32;
#no feature qw(indirect);
use v5.30;
use strict;
use utf8;
use warnings;
use open qw(:std :utf8);
use feature qw(signatures);
no warnings qw(experimental::signatures);


use File::Path qw( make_path );
use File::Basename qw( dirname );

sub replace_slash($){
    my ($prefix) = @_;
    $prefix =~ s</><__>g;
    return $prefix;
}

my ($infile, $outfile, $prefix) = @ARGV;

make_path( dirname( $outfile ));

open my $in, "<", $infile || die;
my $scope = <$in>;

$scope = $scope =~ m<^\s*//\s*scope\s+(.+)>;
$scope = $prefix . replace_slash($1);

# TODO: -DPATCH_LVL=
open my $cpp, "-|", "cpp -Iruntime '$infile' 2>/dev/null" || die;

open my $out, ">", $outfile || die;

# Ugly, but works
while(my $line = <$cpp>){
    $line =~ s(^#)(//);
    $line =~ s<([\w\d/]+)#><$prefix.replace_slash($1)>ge;
    $line =~ s<__><%%>g;
    $line =~ s/\b_/${scope}_/g;
    $line =~ s/%%/__/g;
    print $out $line;
}

