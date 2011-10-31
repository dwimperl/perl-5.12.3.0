package Class::MOP::MiniTrait;
BEGIN {
  $Class::MOP::MiniTrait::AUTHORITY = 'cpan:STEVAN';
}
BEGIN {
  $Class::MOP::MiniTrait::VERSION = '2.0205';
}

use strict;
use warnings;

sub apply {
    my ( $to_class, $trait ) = @_;

    for ( grep { !ref } $to_class, $trait ) {
        Class::MOP::load_class($_);
        $_ = Class::MOP::Class->initialize($_);
    }

    for my $meth ( $trait->get_all_methods ) {
        my $meth_name = $meth->name;

        if ( $to_class->find_method_by_name($meth_name) ) {
            $to_class->add_around_method_modifier( $meth_name, $meth->body );
        }
        else {
            $to_class->add_method( $meth_name, $meth->clone );
        }
    }
}

# We can't load this with use, since it may be loaded and used from Class::MOP
# (via CMOP::Class, etc). However, if for some reason this module is loaded
# _without_ first loading Class::MOP we need to require Class::MOP so we can
# use it and CMOP::Class.
require Class::MOP;

1;

__END__

=pod

=head1 NAME 

Class::MOP::MiniTrait - Extremely limited trait application

=head1 DESCRIPTION

This package provides a single function, C<apply>, which does a half-assed job
of applying a trait to a class. It exists solely for use inside Class::MOP and
L<Moose> core classes.

=cut

