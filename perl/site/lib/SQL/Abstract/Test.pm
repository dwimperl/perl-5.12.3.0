package SQL::Abstract::Test; # see doc at end of file

use strict;
use warnings;
use base qw/Test::Builder::Module Exporter/;
use Data::Dumper;
use Test::Builder;
use SQL::Abstract::Tree;

our @EXPORT_OK = qw/&is_same_sql_bind &is_same_sql &is_same_bind
                    &eq_sql_bind &eq_sql &eq_bind
                    $case_sensitive $sql_differ/;

my $sqlat = SQL::Abstract::Tree->new;

our $case_sensitive = 0;
our $parenthesis_significant = 0;
our $sql_differ; # keeps track of differing portion between SQLs
our $tb = __PACKAGE__->builder;

sub is_same_sql_bind {
  my ($sql1, $bind_ref1, $sql2, $bind_ref2, $msg) = @_;

  # compare
  my $same_sql  = eq_sql($sql1, $sql2);
  my $same_bind = eq_bind($bind_ref1, $bind_ref2);

  # call Test::Builder::ok
  my $ret = $tb->ok($same_sql && $same_bind, $msg);

  # add debugging info
  if (!$same_sql) {
    _sql_differ_diag($sql1, $sql2);
  }
  if (!$same_bind) {
    _bind_differ_diag($bind_ref1, $bind_ref2);
  }

  # pass ok() result further
  return $ret;
}

sub is_same_sql {
  my ($sql1, $sql2, $msg) = @_;

  # compare
  my $same_sql  = eq_sql($sql1, $sql2);

  # call Test::Builder::ok
  my $ret = $tb->ok($same_sql, $msg);

  # add debugging info
  if (!$same_sql) {
    _sql_differ_diag($sql1, $sql2);
  }

  # pass ok() result further
  return $ret;
}

sub is_same_bind {
  my ($bind_ref1, $bind_ref2, $msg) = @_;

  # compare
  my $same_bind = eq_bind($bind_ref1, $bind_ref2);

  # call Test::Builder::ok
  my $ret = $tb->ok($same_bind, $msg);

  # add debugging info
  if (!$same_bind) {
    _bind_differ_diag($bind_ref1, $bind_ref2);
  }

  # pass ok() result further
  return $ret;
}

sub _sql_differ_diag {
  my ($sql1, $sql2) = @_;

  $tb->diag("SQL expressions differ\n"
      ."     got: $sql1\n"
      ."expected: $sql2\n"
      ."differing in :\n$sql_differ\n"
      );
}

sub _bind_differ_diag {
  my ($bind_ref1, $bind_ref2) = @_;

  $tb->diag("BIND values differ\n"
      ."     got: " . Dumper($bind_ref1)
      ."expected: " . Dumper($bind_ref2)
      );
}

sub eq_sql_bind {
  my ($sql1, $bind_ref1, $sql2, $bind_ref2) = @_;

  return eq_sql($sql1, $sql2) && eq_bind($bind_ref1, $bind_ref2);
}


sub eq_bind {
  my ($bind_ref1, $bind_ref2) = @_;

  local $Data::Dumper::Useqq = 1;
  local $Data::Dumper::Sortkeys = 1;

  return Dumper($bind_ref1) eq Dumper($bind_ref2);
}

sub eq_sql {
  my ($sql1, $sql2) = @_;

  # parse
  my $tree1 = $sqlat->parse($sql1);
  my $tree2 = $sqlat->parse($sql2);

  return 1 if _eq_sql($tree1, $tree2);
}

sub _eq_sql {
  my ($left, $right) = @_;

  # one is defined the other not
  if ( (defined $left) xor (defined $right) ) {
    return 0;
  }
  # one is undefined, then so is the other
  elsif (not defined $left) {
    return 1;
  }
  # different amount of elements
  elsif (@$left != @$right) {
    return 0;
  }
  # one is empty - so is the other
  elsif (@$left == 0) {
    return 1;
  }
  # one is a list, the other is an op with a list
  elsif (ref $left->[0] xor ref $right->[0]) {
    $sql_differ = sprintf ("left: %s\nright: %s\n", map { $sqlat->unparse ($_) } ($left, $right) );
    return 0;
  }
  # one is a list, so is the other
  elsif (ref $left->[0]) {
    for (my $i = 0; $i <= $#$left or $i <= $#$right; $i++ ) {
      return 0 if (not _eq_sql ($left->[$i], $right->[$i]) );
    }
    return 1;
  }
  # both are an op-list combo
  else {

    # unroll parenthesis if possible/allowed
    $parenthesis_significant || $sqlat->_parenthesis_unroll($_) for $left, $right;

    # if operators are different
    if ( $left->[0] ne $right->[0] ) {
      $sql_differ = sprintf "OP [$left->[0]] != [$right->[0]] in\nleft: %s\nright: %s\n",
        $sqlat->unparse($left),
        $sqlat->unparse($right);
      return 0;
    }
    # elsif operators are identical, compare operands
    else {
      if ($left->[0] eq 'LITERAL' ) { # unary
        (my $l = " $left->[1][0] " ) =~ s/\s+/ /g;
        (my $r = " $right->[1][0] ") =~ s/\s+/ /g;
        my $eq = $case_sensitive ? $l eq $r : uc($l) eq uc($r);
        $sql_differ = "[$l] != [$r]\n" if not $eq;
        return $eq;
      }
      else {
        my $eq = _eq_sql($left->[1], $right->[1]);
        $sql_differ ||= sprintf ("left: %s\nright: %s\n", map { $sqlat->unparse ($_) } ($left, $right) ) if not $eq;
        return $eq;
      }
    }
  }
}

sub parse { $sqlat->parse(@_) }
1;


__END__

=head1 NAME

SQL::Abstract::Test - Helper function for testing SQL::Abstract

=head1 SYNOPSIS

  use SQL::Abstract;
  use Test::More;
  use SQL::Abstract::Test import => [qw/
    is_same_sql_bind is_same_sql is_same_bind
    eq_sql_bind eq_sql eq_bind
  /];

  my ($sql, @bind) = SQL::Abstract->new->select(%args);

  is_same_sql_bind($given_sql,    \@given_bind,
                   $expected_sql, \@expected_bind, $test_msg);

  is_same_sql($given_sql, $expected_sql, $test_msg);
  is_same_bind(\@given_bind, \@expected_bind, $test_msg);

  my $is_same = eq_sql_bind($given_sql,    \@given_bind,
                            $expected_sql, \@expected_bind);

  my $sql_same = eq_sql($given_sql, $expected_sql);
  my $bind_same = eq_bind(\@given_bind, \@expected_bind);

=head1 DESCRIPTION

This module is only intended for authors of tests on
L<SQL::Abstract|SQL::Abstract> and related modules;
it exports functions for comparing two SQL statements
and their bound values.

The SQL comparison is performed on I<abstract syntax>,
ignoring differences in spaces or in levels of parentheses.
Therefore the tests will pass as long as the semantics
is preserved, even if the surface syntax has changed.

B<Disclaimer> : the semantic equivalence handling is pretty limited.
A lot of effort goes into distinguishing significant from
non-significant parenthesis, including AND/OR operator associativity.
Currently this module does not support commutativity and more
intelligent transformations like Morgan laws, etc.

For a good overview of what this test framework is capable of refer
to C<t/10test.t>

=head1 FUNCTIONS

=head2 is_same_sql_bind

  is_same_sql_bind($given_sql,    \@given_bind,
                   $expected_sql, \@expected_bind, $test_msg);

Compares given and expected pairs of C<($sql, \@bind)>, and calls
L<Test::Builder/ok> on the result, with C<$test_msg> as message. If the test
fails, a detailed diagnostic is printed. For clients which use L<Test::More>,
this is the one of the three functions (L</is_same_sql_bind>, L</is_same_sql>,
L</is_same_bind>) that needs to be imported.

=head2 is_same_sql

  is_same_sql($given_sql, $expected_sql, $test_msg);

Compares given and expected SQL statements, and calls L<Test::Builder/ok> on
the result, with C<$test_msg> as message. If the test fails, a detailed
diagnostic is printed. For clients which use L<Test::More>, this is the one of
the three functions (L</is_same_sql_bind>, L</is_same_sql>, L</is_same_bind>)
that needs to be imported.

=head2 is_same_bind

  is_same_bind(\@given_bind, \@expected_bind, $test_msg);

Compares given and expected bind values, and calls L<Test::Builder/ok> on the
result, with C<$test_msg> as message. If the test fails, a detailed diagnostic
is printed. For clients which use L<Test::More>, this is the one of the three
functions (L</is_same_sql_bind>, L</is_same_sql>, L</is_same_bind>) that needs
to be imported.

=head2 eq_sql_bind

  my $is_same = eq_sql_bind($given_sql,    \@given_bind,
                            $expected_sql, \@expected_bind);

Compares given and expected pairs of C<($sql, \@bind)>. Similar to
L</is_same_sql_bind>, but it just returns a boolean value and does not print
diagnostics or talk to L<Test::Builder>.

=head2 eq_sql

  my $is_same = eq_sql($given_sql, $expected_sql);

Compares the abstract syntax of two SQL statements. Similar to L</is_same_sql>,
but it just returns a boolean value and does not print diagnostics or talk to
L<Test::Builder>. If the result is false, the global variable L</$sql_differ>
will contain the SQL portion where a difference was encountered; this is useful
for printing diagnostics.

=head2 eq_bind

  my $is_same = eq_sql(\@given_bind, \@expected_bind);

Compares two lists of bind values, taking into account the fact that some of
the values may be arrayrefs (see L<SQL::Abstract/bindtype>). Similar to
L</is_same_bind>, but it just returns a boolean value and does not print
diagnostics or talk to L<Test::Builder>.

=head1 GLOBAL VARIABLES

=head2 $case_sensitive

If true, SQL comparisons will be case-sensitive. Default is false;

=head2 $parenthesis_significant

If true, SQL comparison will preserve and report difference in nested
parenthesis. Useful while testing C<IN (( x ))> vs C<IN ( x )>.
Defaults to false;

=head2 $sql_differ

When L</eq_sql> returns false, the global variable
C<$sql_differ> contains the SQL portion
where a difference was encountered.


=head1 SEE ALSO

L<SQL::Abstract>, L<Test::More>, L<Test::Builder>.

=head1 AUTHORS

Laurent Dami, E<lt>laurent.dami AT etat  geneve  chE<gt>

Norbert Buchmuller <norbi@nix.hu>

Peter Rabbitson <ribasushi@cpan.org>

=head1 COPYRIGHT AND LICENSE

Copyright 2008 by Laurent Dami.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.
