#!/usr/bin/perl

use strict;
use warnings;

use File::Path qw(make_path remove_tree);
use File::Basename;
use String::Random;

my $file = $ARGV[0];
my $new = $file =~ s/^(.*)org\-mode(.*)\.org$/$1markdown$2.md/r;
my $dir = dirname($new);

print "$file ==> $new\n";

make_path($dir);


open IN, "<", $file;

my $acc;

#my @code;
#my @code_tokens;
#my @lang;

my %code;

my $harvester = "";
my $in_code=0;
my $lang;

my $string_gen = String::Random->new;

while(<IN>) {
	if(/^\s*#\+BEGIN_SRC (\w+)$/) {
		$harvester="";
		$lang = $1;
		$in_code=1;
	}elsif(/^\s*#\+END_SRC/) {
		$in_code=0;
		my $token = $string_gen->randregex('\w{10}');
		$code{$token} = "```$lang\n$harvester\n```";
		$acc.="$token\n";
	}elsif($in_code) {
		$harvester.=$_;
	}else{
		$acc.=&handle($_);	
	}
}

$acc = &fix_global_things($acc);

for(keys %code) {
	my $token = $_;
	my $block = $code{$token};
	$acc =~ s/$token/$block/;
}

open OUT, ">$new" or die "failed to open file to write";

print OUT $acc;

close IN;
close OUT;


sub handle {
	($_) = @_;
	my $x = "";
	if(/<div class=headline>(.*)<\/div>/) {
		return "# $1\n";
	}elsif(/^\* (.*)$/) {
		return "## $1\n";
	}elsif(/^\*{2} (.*)$/) {
		return "### $1\n";
	}elsif(/^\*{3} (.*)$/) {
		return "#### $1\n";
	}elsif(/^\*{4} (.*)$/) {
		return "##### $1\n";
	}elsif(/^#/) {
		return "";
	}elsif(m{^(\s*)[\+\*](\s+)=?([\w\\]+)=? - \(/(.+?)/\)\s*::\s*(.*?)$}) {
		return &list_spaces($1)."  *$2`$3` ($4) - $5\n";
	}elsif(m{^(\s*)[\+\*](\s+)?([\w\\]+)? - \(/(.+?)/\)\s*::\s*(.*?)$}) {
		return &list_spaces($1)."*$2`$3` ($4) - $5\n";
	}elsif(/^(\s)*\+(.*)/) {
		return "$1* $2\n";
	} else {
		return "$_";
	}
}

sub list_spaces {
	my ($len) = length($_[0]);
	if($len<4) {return " ";}
	if($len>=4) {return "  ";}
	if($len>=6) {return "    ";}
}

sub fix_global_things {
	my ($x) = @_;

	## Internal Links
	$x =~ s/\[\[file:[^\]]*?([^\]\/]*)\.org\]\[([^\]]*)\]\]/[$2]($1.html)/gis;

	## External Links			
	$x =~ s/\[\[([^\]]*?)\]\[([^\]]*?)\]\]/[$2]($1)/gis;

	## Code Snippets
	$x =~ s/=(.+?)=/`$1`/gi;

	## Bold
	$x =~ s/\*([^\s].+?[^\s])\*/**$1**/gi;

	## Italics
	$x =~ s/\(\/(.+?)\/\)/(*$1*) /gi;

	## Escaped underscores
	$x =~ s/\\_/_/gi;

	## Code Blocks
	#$x =~ s/#\+BEGIN_SRC (\w+)(.+?)#\+END_SRC/```$1$2```/gis;

	return $x;

}

