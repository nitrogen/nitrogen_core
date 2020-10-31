#!/usr/bin/perl

use strict;
use warnings;

use File::Path qw(make_path remove_tree);
use File::Basename;
use String::Random;

my $file = $ARGV[0];
my $new = $file =~ s/^(.*)markdown(.*)\.md$/$1html$2.html/r;
my $dir = dirname($new);

print "$file ==> $new\n";

make_path($dir);

system("pandoc -s -f gfm -o $new $file");
