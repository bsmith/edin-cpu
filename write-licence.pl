#!/usr/bin/perl

use 5.024;
use strict;
use warnings;

use Software::License::Perl_5;
my $lic = Software::License::Perl_5->new({
    holder => 'Benjamin Smith',
    year => '2019',
    });

open my $fh, ">", "LICENSE" or die "LICENSE: $!\n";
binmode $fh, ':utf8';
print $fh $lic->fulltext;
close $fh or die "LICENSE: $!\n";
