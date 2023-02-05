#!/usr/bin/perl


&add_wf_anchors("markdown/api.md");


sub add_wf_anchors {
	my ($file) = @_;
	my $contents = &load_file($file);

	my $newcontents = $contents =~ s/(\s*\*\s+)\`wf:(\w+)/$1<a name=\"wf_$2\"><\/a>\`wf:$2/gr;

	open my $fh, ">", $file
		or die "could not open $file for writing: $!";
	print $fh $newcontents;
}


sub load_file {
	my ($file) = @_;
	my $document = do {
		local $/ = undef;
		open my $fh, "<", $file
			or die "could not open $file for reading: $!";
		<$fh>;
	};
	return $document;
}
