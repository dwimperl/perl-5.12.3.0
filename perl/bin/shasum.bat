@rem = '--*-Perl-*--
@echo off
if "%OS%" == "Windows_NT" goto WinNT
perl -x -S "%0" %1 %2 %3 %4 %5 %6 %7 %8 %9
goto endofperl
:WinNT
perl -x -S %0 %*
if NOT "%COMSPEC%" == "%SystemRoot%\system32\cmd.exe" goto endofperl
if %errorlevel% == 9009 echo You do not have Perl in your PATH.
if errorlevel 1 goto script_failed_so_exit_with_non_zero_val 2>nul
goto endofperl
@rem ';
#!perl -w
#line 15

	## shasum: filter for computing SHA digests (ref. sha1sum/md5sum)
	##
	## Copyright (C) 2003-2011 Mark Shelor, All Rights Reserved
	##
	## Version: 5.61
	## Wed Mar  9 05:26:36 MST 2011

	## shasum SYNOPSIS adapted from GNU Coreutils sha1sum.
	## Include an "-a" option for algorithm selection, and a
	## "-p" option for portable digest computation.

my $POD = <<'END_OF_POD';

=head1 NAME

shasum - Print or Check SHA Checksums

=head1 SYNOPSIS

 Usage: shasum [OPTION]... [FILE]...
 Print or check SHA checksums.
 With no FILE, or when FILE is -, read standard input.

   -a, --algorithm   1 (default), 224, 256, 384, 512, 512224, 512256
   -b, --binary      read in binary mode
   -c, --check       read SHA sums from the FILEs and check them
   -p, --portable    read files in portable mode
                         produces same digest on Windows/Unix/Mac
   -t, --text        read in text mode (default)

 The following two options are useful only when verifying checksums:
   -s, --status      don't output anything, status code shows success
   -w, --warn        warn about improperly formatted checksum lines

   -h, --help        display this help and exit
   -v, --version     output version information and exit

 When verifying SHA-512/224 or SHA-512/256 checksums, indicate the
 algorithm explicitly using the -a option, e.g.

   shasum -a 512224 -c checksumfile

 The sums are computed as described in FIPS-180-4.  When checking, the
 input should be a former output of this program.  The default mode is to
 print a line with checksum, a character indicating type (`*' for binary,
 ` ' for text, `?' for portable), and name for each FILE.

 Report shasum bugs to mshelor@cpan.org

=head1 DESCRIPTION

Running I<shasum> is often the quickest way to compute SHA message
digests.  The user simply feeds data to the script through files or
standard input, and then collects the results from standard output.

The following command shows how easy it is to compute digests for typical
inputs such as the NIST test vector "abc":

	perl -e "print qq(abc)" | shasum

Or, if you want to use SHA-256 instead of the default SHA-1, simply say:

	perl -e "print qq(abc)" | shasum -a 256

Since I<shasum> mimics the behavior of the combined GNU I<sha1sum>,
I<sha224sum>, I<sha256sum>, I<sha384sum>, and I<sha512sum> programs,
you can install this script as a convenient drop-in replacement.

=head1 AUTHOR

Copyright (c) 2003-2011 Mark Shelor <mshelor@cpan.org>.

=head1 SEE ALSO

I<shasum> is implemented using the Perl module L<Digest::SHA> or
L<Digest::SHA::PurePerl>.

=cut

END_OF_POD

use strict;
use Fcntl;
use Getopt::Long;

my $VERSION = "5.61";


	## Try to use Digest::SHA.  If not installed, use the slower
	## but functionally equivalent Digest::SHA::PurePerl instead.

my $MOD_PREFER = "Digest::SHA";
my $MOD_SECOND = "Digest::SHA::PurePerl";

my $module = $MOD_PREFER;
eval "require $module";
if ($@) {
	$module = $MOD_SECOND;
	eval "require $module";
	die "Unable to find $MOD_PREFER or $MOD_SECOND\n" if $@;
}


sub usage {
	my($err, $msg) = @_;

	$msg = "" unless defined $msg;
	if ($err) {
		warn($msg . "Type shasum -h for help\n");
		exit($err);
	}
	my($USAGE) = $POD =~ /SYNOPSIS\n\n(.+)\n=head1 DESCRIPTION\n/sm;
	$USAGE =~ s/^ //gm;
	print $USAGE;
	exit($err);
}


	## Sync stdout and stderr by forcing a flush after every write

select((select(STDOUT), $| = 1)[0]);
select((select(STDERR), $| = 1)[0]);


	## Collect options from command line

my ($alg, $binary, $check, $text, $status, $warn, $help, $version);
my ($portable);

eval { Getopt::Long::Configure ("bundling") };
GetOptions(
	'b|binary' => \$binary, 'c|check' => \$check,
	't|text' => \$text, 'a|algorithm=i' => \$alg,
	's|status' => \$status, 'w|warn' => \$warn,
	'h|help' => \$help, 'v|version' => \$version,
	'p|portable' => \$portable
) or usage(1, "");


	## Deal with help requests and incorrect uses

usage(0)
	if $help;
usage(1, "shasum: Ambiguous file mode\n")
	if scalar(grep { defined $_ } ($binary, $portable, $text)) > 1;
usage(1, "shasum: --warn option used only when verifying checksums\n")
	if $warn && !$check;
usage(1, "shasum: --status option used only when verifying checksums\n")
	if $status && !$check;


	## Default to SHA-1 unless overriden by command line option

$alg = 1 unless defined $alg;
grep { $_ == $alg } (1, 224, 256, 384, 512, 512224, 512256)
	or usage(1, "shasum: Unrecognized algorithm\n");


	## Display version information if requested

if ($version) {
	print "$VERSION\n";
	exit(0);
}


	## Try to figure out if the OS is DOS-like.  If it is,
	## default to binary mode when reading files, unless
	## explicitly overriden by command line "--text" or
	## "--portable" options.

my $isDOSish = ($^O =~ /^(MSWin\d\d|os2|dos|mint|cygwin)$/);
if ($isDOSish) { $binary = 1 unless $text || $portable }

my $modesym = $binary ? '*' : ($portable ? '?' : ' ');


	## Read from STDIN (-) if no files listed on command line

@ARGV = ("-") unless @ARGV;


	## sumfile($file): computes SHA digest of $file

sub sumfile {
	my $file = shift;

	my $mode = $portable ? 'p' : ($binary ? 'b' : '');
	my $digest = eval { $module->new($alg)->addfile($file, $mode) };
	if ($@) { warn "shasum: $file: $!\n"; return }
	$digest->hexdigest;
}


	## %len2alg: maps hex digest length to SHA algorithm

my %len2alg = (40 => 1, 56 => 224, 64 => 256, 96 => 384, 128 => 512);
$len2alg{56} = 512224 if $alg == 512224;
$len2alg{64} = 512256 if $alg == 512256;


	## unescape: convert backslashed filename to plain filename

sub unescape {
	$_ = shift;
	s/\\\\/\0/g;
	s/\\n/\n/g;
	return if /\\/;
	s/\0/\\/g;
	return $_;
}


	## verify: confirm the digest values in a checksum file

sub verify {
	my $checkfile = shift;
	my ($err, $fmt_errs, $read_errs, $match_errs) = (0, 0, 0, 0);
	my ($num_lines, $num_files) = (0, 0);
	my ($bslash, $sum, $fname, $rsp, $digest);

	local *FH;
	$checkfile eq '-' and open(FH, '< -')
		and $checkfile = 'standard input'
	or sysopen(FH, $checkfile, O_RDONLY)
		or die "shasum: $checkfile: $!\n";
	while (<FH>) {
		next if /^#/; s/\n$//; s/^[ \t]+//; $num_lines++;
		$bslash = s/^\\//;
		($sum, $modesym, $fname) =
			/^([\da-fA-F]+)[ \t]([ *?])([^\0]*)/;
		$alg = defined $sum ? $len2alg{length($sum)} : undef;
		$fname = unescape($fname) if defined $fname && $bslash;
		if (grep { ! defined $_ } ($alg, $sum, $modesym, $fname)) {
			$alg = 1 unless defined $alg;
			warn("shasum: $checkfile: $.: improperly " .
				"formatted SHA$alg checksum line\n") if $warn;
			$fmt_errs++;
			next;
		}
		$fname =~ s/\r$// unless -e $fname;
		$rsp = "$fname: "; $num_files++;
		($binary, $portable, $text) =
			map { $_ eq $modesym } ('*', '?', ' ');
		unless ($digest = sumfile($fname)) {
			$rsp .= "FAILED open or read\n";
			$err = 1; $read_errs++;
		}
		else {
			if (lc($sum) eq $digest) { $rsp .= "OK\n" }
			else { $rsp .= "FAILED\n"; $err = 1; $match_errs++ }
		}
		print $rsp unless $status;
	}
	close(FH);
	unless ($num_files) {
		$alg = 1 unless defined $alg;
		warn("shasum: $checkfile: no properly formatted " .
			"SHA$alg checksum lines found\n");
		$err = 1;
	}
	elsif (! $status) {
		warn("shasum: WARNING: $fmt_errs line" . ($fmt_errs>1?
		's are':' is') . " improperly formatted\n") if $fmt_errs;
		warn("shasum: WARNING: $read_errs listed file" .
		($read_errs>1?'s':'') . " could not be read\n") if $read_errs;
		warn("shasum: WARNING: $match_errs computed checksum" .
		($match_errs>1?'s':'') . " did NOT match\n") if $match_errs;
	}
	return($err == 0);
}


	## Verify or compute SHA checksums of requested files

my($file, $digest);

my $STATUS = 0;
for $file (@ARGV) {
	if ($check) { $STATUS = 1 unless verify($file) }
	elsif ($digest = sumfile($file)) {
		if ($file =~ /[\n\\]/) {
			$file =~ s/\\/\\\\/g; $file =~ s/\n/\\n/g;
			$digest = "\\$digest";
		}
		print "$digest $modesym", "$file\n";
	}
	else { $STATUS = 1 }
}
exit($STATUS)

__END__
:endofperl
