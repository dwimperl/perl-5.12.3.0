package Test::DatabaseRow;

use strict;
use warnings;

use vars qw($VERSION @EXPORT @ISA %RE $force_utf8);
use Carp;

# set row_ok to be exported
require Exporter;
@ISA = qw(Exporter);
@EXPORT = qw(row_ok not_row_ok);

# set the version number
$VERSION = "1.04";

# okay, try loading Regexp::Common
eval { require Regexp::Common; Regexp::Common->import };

# if we couldn't load Regexp::Common then we use the one regex that I
# copied and pasted from there that we need.  We could *always* do
# this, but at least this way if someone cares enough they can upgrade
# Regexp::Common when it changes and they don't have to wait for me to
# upgrade this module too

if ($@)
{
  # this regexp written by Damian Conway
  $RE{num}{real} = qr/(?:(?i)(?:[+-]?)(?:(?=[0123456789]|[.])
                      (?:[0123456789]*)(?:(?:[.])(?:[0123456789]{0,}))?)
                      (?:(?:[E])(?:(?:[+-]?)(?:[0123456789]+))|))/x;
}

=head1 NAME

Test::DatabaseRow - simple database tests

=head1 SYNOPSIS

 use Test::More tests => 3;
 use Test::DatabaseRow;

 # set the default database handle
 local $Test::DatabaseRow::dbh = $dbh;

 # sql based test
 row_ok( sql   => "SELECT * FROM contacts WHERE cid = '123'",
         tests => [ name => "trelane" ],
         label => "contact 123's name is trelane");

 # test with shortcuts
 row_ok( table => "contacts",
         where => [ cid => 123 ],
         tests => [ name => "trelane" ],
         label => "contact 123's name is trelane");

 # complex test
 row_ok( table => "contacts",
         where => { '='    => { name   => "trelane"            },
                    'like' => { url    => '%shortplanks.com'   },},

         tests => { '=='   => { cid    => 123,
                                num    => 134                  },
                    'eq'   => { person => "Mark Fowler"        },
                    '=~'   => { road   => qr/Liverpool R.?.?d/ },},

         label => "trelane entered into contacts okay" );

=head1 DESCRIPTION

This is a simple module for doing very very simple quick tests on a
database, primarily designed to test if a row exists with the correct
details in a table or not.  For more advanced testing (joins, etc) it's
probably easier for you to roll your own tests by hand than use this
module.

Exports one test function C<row_ok>.  This tests if the first selected
row compares to the passed specification.

The C<row_ok> takes named attributes that control which rows in which
table it selects, and what tests are carried out on those rows.

=over 4

=item dbh

The database handle that the test should use.  In lieu of this
attribute being passed the test will use whatever handle is set
in the C<$Test::DatabaseRow::dbh> global variable.

=item sql

The sql to select the rows you want to check.  Some people may prefer
to use the C<table> and C<where> arguments (see below) to have the
function build their SQL dynamically for them.  This can either be
just a plain string, or it can be an array ref of which the first
element should contain the SQL with placeholders and the remaining
elements will be considered bind variables

  # using the plain string version
  row_ok(sql   => "SELECT * FROM contacts WHERE cid = '123'",
         tests => [ name => "Trelane" ]);

  # using placeholders and bind variables
  row_ok(sql   => [ "SELECT * FROM contacts WHERE cid = ?", 123 ],
         tests => [ name => "Trelane" ]);

=item table

If you're not using the sql option, then the name of the table the
select should be carried out on.

=item where

If you're not using the sql option, then a collection of things
that you want combined into a WHERE clause in order to select the row
that you want to test.

This is normally a hash of hashes.  It's a hashref keyed by SQL
comparison operators that has in turn values that are further hashrefs
of column name and values pairs.  This sounds really complicated, but
is quite simple once you've been shown an example.  If we could get
get the data to test with a SQL like so:

  SELECT * FROM foo
    WHERE foo  =    'bar'     AND
          baz  =     23       AND
          fred LIKE 'wilma%'  AND
          age  >=    18

Then we could have the function build that SQL like so:

  row_ok(table => "tablename",
         where => { '='    => { foo  => "bar",
                                baz  => 23,       },
                    'LIKE' => { fred => 'wimla%', },
                    '>='   => { age  => '18',     },});

Note how each different type of comparison has it's own little hashref
containing the column name and the value for that column that the
associated operator SQL should search for.

This syntax is quite flexible, but can be overkill for simple tests.
In order to make this simpler, if you are only using '=' tests you
may just pass an arrayref of the columnnames / values.  For example,
just to test

  SELECT * FROM tablename
    WHERE foo  =     bar     AND
          baz  =     23;

You can simply pass

  row_ok(table => "tablename",
         where => [ foo  => "bar",
                    baz  => 23,    ]);

Which, in a lot of cases, makes things a lot quicker and simpler to
write.

NULL values can confuse things in SQL.  All you need to remember is that
when building SQL statements use C<undef> whenever you want to use a
NULL value.  Don't use the string "NULL" as that'll be interpreted as
the literal string made up of a N, a U and two Ls.

As a special case, using C<undef> either in a C<=> or in the short
arrayref form will cause a "IS" test to be used instead of a C<=> test.
This means the statements:

  row_ok(table => "tablename",
         where => [ foo  => undef ],)

Will produce:

  SELECT * FROM tablename
    WHERE foo IS NULL

=item tests

The comparisons that you want to run between the expected data and the
data in the first line returned from the database.  If you do not
specify any tests then the test will simply check if I<any> row is
returned from the database.

Normally this is a hash of hashes in a similar vein to C<where>.
This time the outer hash is keyed by Perl comparison operators, and
the inner hashes contain column names and the expected values for
these columns.  For example:

  row_ok(sql   => $sql,
         tests => { "eq" => { wibble => "wobble",
                              fish   => "fosh",    },
                    "==" => { bob    => 4077       },
                    "=~" => { fred   => qr/barney/ },},);

This checks that the column wibble is the string "wobble", column fish
is the string "fosh", column bob is equal numerically to 4077, and
that fred contains the text "barney".  You may use any infix
comparison operator (e.g. "<", ">", "&&", etc, etc) as a test key.

The first comparison to fail (to return false) will cause the whole
test to fail, and debug information will be printed out on that comparison.

In a similar fashion to C<where> you can also pass a arrayref for
simple comparisons.  The function will try and Do The Right Thing with
regard to the expected value for that comparison.  Any expected value that
looks like a number will be compared numerically, a regular expression
will be compared with the C<=~> operator, and anything else will
undergo string comparison.  The above example therefore could be
rewritten:

  row_ok(sql   => $sql,
         tests => [ wibble => "wobble",
                    fish   => "fosh",
                    bob    => 4077,
                    fred   => qr/barney/ ]);

=item verbose

Setting this option to a true value will cause verbose diagnostics to
be printed out during any failing tests.  You may also enable this
feature by setting either C<$Test::DatabaseRow::verbose> variable the
C<TEST_DBROW_VERBOSE> environmental variable to a true value.

=item store_rows

Sometimes, it's not enough to just use the simple tests that
B<Test::DatabaseRow> offers you.  In this situation you can use the
C<store_rows> function to get at the results that row_ok has extacted
from the database.  You should pass a reference to an array for the
results to be stored in;  After the call to C<row_ok> this array
will be populated with one hashref per row returned from the database,
keyed by column names.

  row_ok(sql => "SELECT * FROM contact WHERE name = 'Trelane'",
         store_rows => \@rows);

  ok(Email::Valid->address($rows[0]{'email'}));

=item store_row

The same as C<store_rows>, but only the stores the first row returned
in the variable.  Instead of passing in an array reference you should
pass in either a reference to a hash...

  row_ok(sql => "SELECT * FROM contact WHERE name = 'Trelane'",
         store_rows => \%row);

  ok(Email::Valid->address($row{'email'}));

...or a reference to a scalar which should be populated with a
hashref...

  row_ok(sql => "SELECT * FROM contact WHERE name = 'Trelane'",
         store_rows => \$row);

  ok(Email::Valid->address($row->{'email'}));

=back

=head2 Checking the number of results

By default C<row_ok> just checks the first row returned from the
database matches the criteria passed.  By setting the parameters below
you can also cause the module to check that the correct number of rows
are returned from by the select statment (though only the first row
will be tested against the test conditions.)

=over 4

=item results

Setting this parameter causes the test to ensure that the database
returns exactly this number of rows when the select statement is
executed.  Setting this to zero allows you to ensure that no matching
rows were found by the database, hence this parameter can be used
for negative assertions about the database.

  # assert that Trelane is _not_ in the database
  row_ok(sql     => "SELECT * FROM contacts WHERE name = 'Trelane'",
         results => 0 );

  # convience function that does the same thing
  not_row_ok(sql => "SELECT * FROM contacts WHERE name = 'Trelane'")

=item min_results / max_results

This parameter allows you to test that the database returns
at least or no more than the passed number of rows when the select
statement is executed.

=back

=cut

sub row_ok
{
  my %args = @_;

  # the database handle
  $args{dbh} ||= $Test::DatabaseRow::dbh
    or croak "No dbh passed and no default dbh set";

  # do we need to load the Encode module?  Don't do this
  # unless we really have to
  if (($args{force_utf8} || $force_utf8) && !$INC{"Encode.pm"})
    { eval "use Encode" }

  my @data;
  eval
  {
    # all problems with the database are fatal
    my $old = $args{dbh}{RaiseError};
    $args{dbh}{RaiseError} = 1;

    # get the SQL and execute it
    ($args{sqls}, $args{sqlb}) = _build_select(%args);
    my $sth = $args{dbh}->prepare($args{sqls});
    $sth->execute(@{ $args{sqlb} });

    # store the results
    while (1)
    {
      # get the data from the database
      my $data = $sth->fetchrow_hashref;

      # we done here?  No more data in database
      last unless defined $data;

      # munge the utf8 flag if we need to
      if ($args{force_utf8} || $force_utf8)
        { Encode::_utf8_on($_) foreach values %{ $data } }

      # store the data
      push @data, $data;
    }

    # retore the original error handling
    $args{dbh}{RaiseError} = $old;
  };

  # re-throw errors from our caller's perspective
  if ($@) { croak $@ }

  # store the results in the passed data structure if there is
  # one.  We can use the actual data structures as control won't
  # return to the end of the routine.  In theory some really weird
  # stuff could happen if this was a a shared variable between
  # multiple threads, but let's just hope nothing does that.

  if ($args{store_rows})
  {
    croak "Must pass an arrayref in 'store_rows'"
      unless ref($args{store_rows}) eq "ARRAY";
    @{ $args{store_rows} } = @data;
  }

  if ($args{store_row})
  {
    if (ref($args{store_row}) eq "HASH")
      { %{ $args{store_row} } = %{ $data[0] } }
    else
    {
      eval
      {
	${ $args{store_row} } = $data[0];
      };

      if ($@)
      {
	if ($@ =~ /Not a SCALAR reference/)
          { croak "Must pass a scalar or hash reference with 'store_row'" }
        else
	  { die $@ }
      }
    }
  }

  # perform tests on the data

  my $nrows = @data;
  # fail the test if we're running just one test and no matching row was returned
  if(!defined($args{min_results}) &&
     !defined($args{max_results}) &&
     !defined($args{results}) &&
     $nrows == 0)
  {
    Test::Builder::DatabaseRow->ok(0,$args{label} || "simple db test");
    Test::Builder::DatabaseRow->diag("No matching row returned");
    _sql_diag(%args);
    return 0;
  }

  # check we got the exected number of rows back if they specified exactly
  if(defined($args{results}) && $nrows != $args{results})
  {
    Test::Builder::DatabaseRow->ok(0,$args{label} || "simple db test");
    Test::Builder::DatabaseRow->diag("Got the wrong number of rows back from the database.");
    Test::Builder::DatabaseRow->diag("  got:      $nrows rows back");
    Test::Builder::DatabaseRow->diag("  expected: $args{results} rows back");
    _sql_diag(%args);
    return 0;
  }

  # check we got enough matching rows back
  if(defined($args{min_results}) && $nrows < $args{min_results})
  {
    Test::Builder::DatabaseRow->ok(0,$args{label} || "simple db test");
    Test::Builder::DatabaseRow->diag("Got too few rows back from the database.");
    Test::Builder::DatabaseRow->diag("  got:      $nrows rows back");
    Test::Builder::DatabaseRow->diag("  expected: $args{min_results} rows or more back");
    _sql_diag(%args);
    return 0;
  }

  # check we got didn't get too many matching rows back
  if(defined($args{max_results}) && $nrows > $args{max_results})
  {
    Test::Builder::DatabaseRow->ok(0,$args{label} || "simple db test");
    Test::Builder::DatabaseRow->diag("Got too many rows back from the database.");
    Test::Builder::DatabaseRow->diag("  got:      $nrows rows back");
    Test::Builder::DatabaseRow->diag("  expected: $args{max_results} rows or fewer back");
    _sql_diag(%args);
    return 0;
  }

  my $tests = $args{tests}
    or return
      Test::Builder::DatabaseRow->ok(1,$args{label} || "simple db test");

  # is this a dtrt operator?  If so, call _munge_array to
  # make it into a hashref if that's possible
  if (ref $tests eq "ARRAY")
    { eval { $tests = _munge_array($tests) }; croak $@ if $@ }

  # check we've got a hash
  unless (ref($tests) eq "HASH")
    { croak "Can't understand the argument passed in 'tests'" }

  # pull the first line off the data list
  my $data = shift @data;

  # now for each test
  foreach my $oper (sort keys %$tests)
  {
    my $valuehash = $tests->{ $oper };

    # check it's a hashref (hopefully of colnames/expected vals)
    unless (ref($valuehash) eq "HASH")
      { croak "Can't understand the argument passed in 'tests'" }

    # process each entry in that hashref
    foreach my $colname (sort keys %$valuehash)
    {
      # work out what we expect
      my $expect = $valuehash->{ $colname };
      my $got    = $data->{ $colname };

      unless (exists($data->{ $colname }))
      {
        croak "No column '$colname' returned from sql" if $args{sql};
        croak "No column '$colname' returned from table '$args{table}'";
      }

      # try the comparison
      my $ok = do {
                     # disable warnings as we might compare undef
	             local $SIG{__WARN__} = sub {}; # $^W not work

		     # do a string eval
                     eval "\$got $oper \$expect"
                  };

      # print the error if there was an error
      unless( $ok )
      {
	Test::Builder::DatabaseRow->ok(0,$args{label} || "simple db test");
	Test::Builder::DatabaseRow->diag("While checking column '$colname'\n");
        if( $oper =~ /^(eq|==)$/ )
	{
	  Test::Builder::DatabaseRow->_is_diag($got, $oper, $expect);
	  _sql_diag(%args);
	  return 0;
        }
        else
	{
	  Test::Builder::DatabaseRow->_cmp_diag($got, $oper, $expect);
	  _sql_diag(%args);
	  return 0;
        }
      }
    }
  }

  # okay, got this far, must have been okay
  Test::Builder::DatabaseRow->ok(1,$args{label} || "simple db test");
}

sub _munge_array
{
  # get the array of tests
  my @tests = @{ shift() };

  # new place where we're storing our freshly created hash
  my $newtests = {};

  if (@tests % 2 != 0)
    { die "Can't understand the passed test arguments" }

  # for each key/value pair
  while (@tests)
  {
    my $key   = shift @tests;
    my $value = shift @tests;

    # set the comparator based on the type of value we're comparing
    # against.  This can lead to some annoying cases, but if they
    # want proper comparison they can use the non dwim mode

    if (!defined($value))
    {
      $newtests->{'eq'}{ $key } = $value;
    }
    elsif (ref($value) eq "Regexp")
    {
      $newtests->{'=~'}{ $key } = $value;
    }
    elsif ($value =~ /^$RE{num}{real}/)
    {
      $newtests->{'=='}{ $key } = $value;
    }
    else
    {
      # default to string comparison
      $newtests->{'eq'}{ $key } = $value;
    }
  }

  return $newtests;
}

# build a sql statement
sub _build_select
{
  my %args = @_;

  # can we ignore all of this and just used the passed select?
  if ($args{sql})
  {
    # is a multi-thingy wosit?
    if (ref($args{sql}) eq "ARRAY")
      { return shift @{$args{sql}}, $args{sql} }

    # just return the whole sql
    return ($args{sql}, []);
  }

  my $select = "SELECT * FROM ";

  ###
  # the table
  ###

  my $table = $args{table}
   or die "No 'table' passed as an argument";

  $select .= $table . " ";

  ###
  # the where clause
  ###

  my $where = $args{where}
   or die "No 'where' passed as an argument";

  # convert it all to equals tests if we were using the
  # shorthand notation
  if (ref $where eq "ARRAY")
  {
    $where = { "=" => { @{$where} }, };
  }

  # check we've got a hash
  unless (ref($where) eq "HASH")
    { die "Can't understand the argument passed in 'where'" }

  $select .= "WHERE ";
  my @conditions;
  foreach my $oper (sort keys %$where)
  {
    my $valuehash = $where->{ $oper };

    unless (ref($valuehash) eq "HASH")
      { die "Can't understand the argument passed in 'where'" }

    foreach my $field (sort keys %$valuehash)
    {
      # get the value
      my $value = $valuehash->{ $field };

      # should this be "IS NULL" rather than "= ''"
      if ($oper eq "=" && !defined($value))
      {
	push @conditions, "$field IS NULL";
      }
      elsif (!defined($value))
      {
	# just an undef.  I hope $oper is "IS" or "IS NOT"
	push @conditions, "$field $oper NULL";
      }
      else
      {
	# proper value, quote it properly
	push @conditions, "$field $oper ".$args{dbh}->quote($value);
      }
    }
  }

  $select .= join ' AND ', @conditions;
  return $select;
}

# returns true iff we should be printing verbose diagnostic messages
sub _verbose
{
  my %args = @_;
  return $args{verbose}
         || $Test::DatabaseRow::verbose
	 || $ENV{TEST_DBROW_VERBOSE};
}

# prints out handy diagnostic text if we're printing out verbose text
sub _sql_diag
{
  my %args = @_;
  return unless _verbose(%args);

  # print out the SQL
  Test::Builder::DatabaseRow->diag("The SQL executed was:");
  Test::Builder::DatabaseRow->diag(map { "  $_\n" } split /\n/, $args{sqls});

  # print out the bound parameters
  if (@{ $args{sqlb} })
  {
    Test::Builder::DatabaseRow->diag("The bound parameters were:");
    foreach my $bind (@{ $args{sqlb} })
    {
      if (defined($bind))
       { Test::Builder::DatabaseRow->diag("  '$bind'") }
      else
       { Test::Builder::DatabaseRow->diag("  undef") }
    }
  }

  # print out the database
  Test::Builder::DatabaseRow->diag("on database '$args{dbh}{Name}'");
}

sub not_row_ok
{
  local $Test::Builder::Level = $Test::Builder::Level + 1;
  row_ok(@_, results => 0);
}

=head2 Other SQL modules

The SQL creation routines that are part of this module are designed
primarily with the concept of getting simple single rows out of the
database with as little fuss as possible.  This having been said, it's
quite possible that you need to use a more complicated SQL generation
scheme than the one provided.

This module is designed to work (hopefully) reasonably well with the
other modules on CPAN that can automatically create SQL for you.  For
example, B<SQL::Abstract> is a module that can manufacture much more
complex select statements that can easily be 'tied in' to C<row_ok>:

  use SQL::Abstract;
  use Test::DatabaseRow;
  my $sql = SQL::Abstract->new();

  # more complex routine to find me heuristically by looking
  # for any one of my nicknames and my street address
  row_ok(sql   => [ $sql->select("contacts",
                                 "*",
                                 { name => [ "Trelane",
                                             "Trel",
                                             "MarkF" ],
                                   road => { 'like' => "Liverpool%" },
                                 })],
         tests => [ email => 'mark@twoshortplanks.com' ],
         label => "check mark's email address");

=head2 utf8 hacks

Often, you may store data utf8 data in your database.  However, many
modern databases still do not store the metadata to indicate the data
stored in them is utf8 and thier DBD drivers may not set the utf8 flag
on values returned to Perl.  This means that data returned to Perl
will be treated as if it is encoded in your normal charecter set
rather than being encoded in utf8 and when compared to a byte for
byte an identical utf8 string may fail comparison.

    # this will fail incorrectly on data coming back from
    # mysql since the utf8 flags won't be set on returning data
    use utf8;
    row_ok(sql   => $sql,
           tests => [ name => "Napol\x{e9}on" ]);

The solution to this is to use C<Encode::_utf_on($value)> on each
value returned from the database, something you will have to do
yourself in your application code.  To get this module to do this for
you you can either pass the C<force_utf8> flag to C<row_ok>.

    use utf8;
    row_ok(sql        => $sql,
           tests      => [ name => "Napol\x{e9}on" ],
           force_utf8 => 1);

Or set the global C<$Test::DatabaseRow::force_utf8> variable

   use utf8;
   local $Test::DatabaseRow::force_utf8 = 1;
   row_ok(sql        => $sql,
          tests      => [ name => "Napol\x{e9}on" ]);

Please note that in the above examples with C<use utf8> enabled I
could have typed unicode eacutes into the string directly rather than
using the C<\x{e9}> escape sequence, but alas the pod renderer you're
using to view this documentation would have been unlikely to render
those examples correctly, so I didn't.

Please also note that if you want the debug information that this
module creates to be redered to STDERR correctly for your utf8
terminal then you may need to stick

   binmode STDERR, ":utf8";

At the top of your script.

=head1 BUGS

You I<must> pass a C<sql> or C<where> argument to limit what is
returned from the table.  The case where you don't want to is so
unlikely (and it's much more likely that you've written a bug in your
test script) that omitting both of these is treated as an error.  If
you I<really> need to not pass a C<sql> or C<where> argument, do C<< where
=> [ 1 => 1 ] >>.

We currently only test the first line returned from the database.
This probably could do with rewriting so we test all of them.  The
testing of this data is the easy bit; Printing out useful diagnostic
infomation is hard.  Patches welcome.

Passing shared variables (variables shared between multiple threads
with B<threads::shared>) in with C<store_row> and C<store_rows> and
then changing them while C<row_ok> is still executing is just asking
for trouble.

The utf8 stuff only really works with perl 5.8 and later.  It just
goes horribly wrong on earlier perls.  There's nothing I can do to
correct that.  Also, no matter what version of Perl you're running,
currently no way provided by this module to force the utf8 flag to be
turned on for some fields and not on for others.

=head1 AUTHOR

Written by Mark Fowler L<gt>mark@twoshortplanks.comE<gt>.  Copyright
Profero 2003, 2004.

Some code taken from B<Test::Builder>, written by Michael Schwern.
Some code taken from B<Regexp::Common>, written by Damian Conway.  Neither
objected to it's inclusion in this module.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

Bugs (and requests for new features) can be reported to the open source
development team at Profero though the CPAN RT system:
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Test-DatabaseRow>

=head1 SEE ALSO

L<Test::More>, L<DBI>

=cut

1;

package Test::Builder::DatabaseRow;

use strict;
use vars qw($Test);

# get the test builder singleton
use Test::Builder;
$Test = Test::Builder->new();

# replicate the testing functions we use

sub ok
{
  local $Test::Builder::Level = $Test::Builder::Level + 1;
  shift;
  return $Test->ok(@_);
}

sub diag
{
  local $Test::Builder::Level = $Test::Builder::Level + 1;
  shift;
  return $Test->diag(@_);
}

# _cmp_diag and _is_diag were originally private functions in
# Test::Builder (and were written by Schwern).  In theory we could
# call them directly there and it should make no difference but since
# they are private functions they could change at any time (or even
# vanish) as new versions of Test::Builder are released.  To protect
# us from that happening we've defined them here.

sub _cmp_diag {
    my($self, $got, $type, $expect) = @_;

    $got    = defined $got    ? "'$got'"    : 'undef';
    $expect = defined $expect ? "'$expect'" : 'undef';

    return $Test->diag(sprintf <<DIAGNOSTIC, $got, $type, $expect);
    %s
        %s
    %s
DIAGNOSTIC
}

sub _is_diag {
    my($self, $got, $type, $expect) = @_;

    foreach my $val (\$got, \$expect) {
        if( defined $$val ) {
            if( $type eq 'eq' ) {
                # quote and force string context
                $$val = "'$$val'"
            }
            else {
                # force numeric context
                $$val = $$val+0;
            }
        }
        else {
            $$val = 'NULL';
        }
    }

    return $Test->diag(sprintf <<DIAGNOSTIC, $got, $expect);
         got: %s
    expected: %s
DIAGNOSTIC

}
1;
