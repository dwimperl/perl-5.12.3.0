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
#!/usr/bin/env perl
#line 15
# vim: set ft=perl:

# -------------------------------------------------------------------
# Copyright (C) 2002-2009 The SQLFairy Authors
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; version 2.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
# 02111-1307  USA
# -------------------------------------------------------------------

=head1 NAME

sqlt-diff - find the differences b/w two schemas

=head1 SYNOPSIS

For help:

  sqlt-diff -h|--help

For a list of all valid parsers:

  sqlt -l|--list

To diff two schemas:

  sqlt-diff [options] file_name1=parser1 file_name2=parser2

Options:

  -d|--debug   Show debugging info
  -t|--trace   Turn on tracing for Parse::RecDescent
  -c|--case-insensitive   Compare tables/columns case-insensitively
  --ignore-index-names    Ignore index name differences
  --ignore-constraint-names   Ignore constraint name differences
  --mysql_parser_version=<#####> Specify a target MySQL parser version
                                 for dealing with /*! comments
  --output-db=<Producer>  This Producer will be used instead of one
                          corresponding to parser1 to format output
                          for new tables
  --ignore-view-sql    Ignore view SQL differences
  --ignore-proc-sql    Ignore procedure SQL differences
  --no-batch-alters    Do not clump multile alters to the same table into a
                       single ALTER TABLE statement where possible.

=head1 DESCRIPTION

sqlt-diff is a utility for creating a file of SQL commands necessary to
transform the first schema provided to the second.  While not yet 
exhaustive in its ability to mutate the entire schema, it will report the 
following

=over

=item * New tables

Using the Producer class of the target (second) schema, any tables missing
in the first schema will be generated in their entirety (fields, constraints,
indices).

=item * Missing/altered fields

Any fields missing or altered between the two schemas will be reported 
as:

  ALTER TABLE <table_name> 
    [DROP <field_name>] 
    [CHANGE <field_name> <datatype> (<size>)] ;

=item * Missing/altered indices

Any indices missing or of a different type or on different fields will be
indicated.  Indices that should be dropped will be reported as such:
 
  DROP INDEX <index_name> ON <table_name> ;

An index of a different type or on different fields will be reported as a 
new index as such:

  CREATE [<index_type>] INDEX [<index_name>] ON <table_name> 
    ( <field_name>[,<field_name>] ) ;

=back

ALTER, CREATE, DROP statements are created by
SQL::Translator::Producer::*, see there for support/problems.

Currently (v0.0900), only MySQL is supported by this code.

=cut

# -------------------------------------------------------------------

use strict;
use warnings;
use Pod::Usage;
use Data::Dumper;
use SQL::Translator;
use SQL::Translator::Diff;
use SQL::Translator::Schema::Constants;

use vars qw( $VERSION );
$VERSION = '1.59';

my ( @input, $list, $help, $debug, $trace, $caseopt, $ignore_index_names, 
	$ignore_constraint_names, $output_db, $mysql_parser_version,
	$ignore_view_sql, $ignore_proc_sql, $no_batch_alters );
for my $arg ( @ARGV ) {
    if ( $arg =~ m/^-?-l(ist)?$/ ) {
        $list = 1;
    }
    elsif ( $arg =~ m/^-?-h(elp)?$/ ) {
        $help = 1;
    }
    elsif ( $arg =~ m/^-?-d(ebug)?$/ ) {
        $debug = 1; 
    }
    elsif ( $arg =~ m/^-?-t(race)?$/ ) {
        $trace = 1; 
    }
    elsif ( $arg =~ m/^-?-c(ase-insensitive)?$/ ) {
        $caseopt = 1; 
    }
    elsif ( $arg =~ m/^--ignore-index-names$/ ) {
        $ignore_index_names = 1; 
    }
    elsif ( $arg =~ m/^--ignore-constraint-names$/ ) {
        $ignore_constraint_names = 1; 
    }
    elsif ( $arg =~ m/^--mysql-parser-version=(.+)$/ ) {
        $mysql_parser_version = $1; 
    }
    elsif ( $arg =~ m/^--output-db=(.+)$/ ) {
        $output_db = $1; 
    }
    elsif ( $arg =~ m/^--ignore-view-sql$/ ) {
        $ignore_view_sql = 1; 
    }
    elsif ( $arg =~ m/^--ignore-proc-sql$/ ) {
        $ignore_proc_sql = 1; 
    }
    elsif ( $arg =~ m/^([^=]+)=(.+)$/ ) {
        push @input, { file => $1, parser => $2 };
    }
    elsif ( $arg =~ m/^--no-batch-alters$/ ) {
      $no_batch_alters = 1;
    }
    else {
        pod2usage( msg => "Unknown argument '$arg'" );
    }
}

print STDERR <<'EOM';
This code is experimental, currently the new code only supports MySQL or 
SQLite diffing. To add support for other databases, please patch the relevant
SQL::Translator::Producer:: module.  If you need compatibility with the old
sqlt-diff, please use sqlt-diff-old, and look into helping us make this one
work for you
EOM

pod2usage(1) if $help || !@ARGV;
pod2usage('Please specify only two schemas to diff') if scalar @input > 2;

my $tr            = SQL::Translator->new;
my @parsers       = $tr->list_parsers;
my %valid_parsers = map { $_, 1 } @parsers;

if ( $list ) {
    print "\nParsers:\n", map { "\t$_\n" } sort @parsers;
    print "\n";
    exit(0);
}

pod2usage( msg => 'Too many file args' ) if @input > 2;

my ( $source_schema, $source_db, $target_schema, $target_db ) = map {
    my $file   = $_->{'file'};
    my $parser = $_->{'parser'};

    die "Unable to read file '$file'\n" unless -r $file;
    die "'$parser' is an invalid parser\n" unless $valid_parsers{ $parser };

    my $t = SQL::Translator->new(parser_args => {mysql_parser_version => $mysql_parser_version});
    $t->debug( $debug );
    $t->trace( $trace );
    $t->parser( $parser )            or die $tr->error;
    my $out = $t->translate( $file ) or die $tr->error;
    my $schema = $t->schema;
    unless ( $schema->name ) {
        $schema->name( $file );
    }

    ($schema, $parser);
} @input;

my $result = SQL::Translator::Diff::schema_diff($source_schema, $source_db, 
                                                $target_schema, $target_db,
                                                { caseopt                 => $caseopt,
                                                  ignore_index_names      => $ignore_index_names,
                                                  ignore_constraint_names => $ignore_constraint_names,
                                                  ignore_view_sql         => $ignore_view_sql,
                                                  ignore_proc_sql         => $ignore_proc_sql,
                                                  output_db               => $output_db,
                                                  no_batch_alters         => $no_batch_alters,
                                                  debug                   => $debug,
                                                  trace                   => $trace });
if($result)
{
    print $result;
}
else
{
    print "No differences found.";
}

# -------------------------------------------------------------------
# Bring out number weight & measure in a year of dearth.
# William Blake
# -------------------------------------------------------------------

=pod

=head1 AUTHOR

Ken Youens-Clark E<lt>kclark@cpan.orgE<gt>.

=head1 SEE ALSO

SQL::Translator, L<http://sqlfairy.sourceforge.net>.

=cut

__END__
:endofperl
