package Test::Trap::Builder::PerlIO;

use version; $VERSION = qv('0.2.1');

use strict;
use warnings;
use Test::Trap::Builder;
use PerlIO 'scalar';

sub import {
  Test::Trap::Builder->output_layer_backend( perlio => $_ ) for sub {
    my $self = shift;
    my ($name, $fileno, $globref) = @_;
    local *$globref;
    {
      no warnings 'io';
      open *$globref, '>', \$self->{$name};
    }
    $self->Next;
  };
}

1; # End of Test::Trap::Builder::PerlIO

__END__

=head1 NAME

Test::Trap::Builder::PerlIO - Output layer backend using PerlIO::scalar

=head1 VERSION

Version 0.2.1

=head1 DESCRIPTION

This module provides an implementation I<perlio>, based on
PerlIO::scalar, for the trap's output layers.  Note that you may
specify different implementations for each output layer on the trap.

See also L<Test::Trap> (:stdout and :stderr) and
L<Test::Trap::Builder> (output_layer).

=head1 CAVEATS

These layers use in-memory files, and so will not (indeed cannot) trap
output from forked-off processes -- including system() calls.

Threads?  No idea.  It might even work correctly.

=head1 BUGS

Please report any bugs or feature requests directly to the author.

=head1 AUTHOR

Eirik Berg Hanssen, C<< <ebhanssen@allverden.no> >>

=head1 COPYRIGHT & LICENSE

Copyright 2006-2007 Eirik Berg Hanssen, All Rights Reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
