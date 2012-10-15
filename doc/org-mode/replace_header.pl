#!/usr/bin/perl

# Usage: ./replace_header.pl [HeaderSource] [Files to update]
# Example: ./replace_header.pl headers/elements_header.org.src elements/*.org

if($#ARGV < 2) {
	die "Usage: ./replace_header.pl [HeaderSourceFile] [Target1, Target2, ... ]\n";
}

my ($src, @files) = @ARGV;

open FILE, $src or die "Couldn't open file $src";
my ($new_header) = <FILE>;
close FILE;

foreach $file (@files) {
	my $neworg = "";
	print "...$file\n";
	open NEWORG, ">$file.new";
	open ORG, "<$file";
	while(<ORG>) {
		if(/^#\+TEXT:.*API.*Elements.*Actions/) {
			print NEWORG $new_header;
		} else {
			print NEWORG;
		}
	}
	close NEWORG;
	close ORG;
	rename "$file.new", $file;
}
