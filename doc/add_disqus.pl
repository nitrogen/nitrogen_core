#!/usr/bin/perl

use strict;


my $raw_inject = &read_file("disqus.html");
my @files = `find html/* | grep \\.html\$`;

for my $file (@files) {
	chomp($file);
	print "Injecting Disqus code into $file...";
	my $to_inject = $raw_inject;
	$to_inject =~ s/<page-identifier>/$file/g;
	my $contents = &read_file($file);
	if($contents =~ /<div id="disqus_thread">/) {
		print "already done\n";
	}else{
		$contents =~ s!</div>[\s\n]*</body>!</div>$to_inject</body>!g;
		&write_file($file,$contents);
		print "done\n";
	}
}

sub write_file
{
	my ($filename,$contents) = @_;
	open my $fh, ">$filename" or die "error opening $filename: $!";
	print $fh $contents;
	close $fh;
}


sub read_file
{
	my ($filename) = @_;
	open my $fh, "<$filename" or die "error opening $filename: $!";
	my $contents = do { local $/; <$fh> };
	close $fh;
	return $contents;
}
