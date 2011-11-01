package Excel::Writer::XLSX::Package::SharedStrings;

###############################################################################
#
# SharedStrings - A class for writing the Excel XLSX sharedStrings file.
#
# Used in conjunction with Excel::Writer::XLSX
#
# Copyright 2000-2011, John McNamara, jmcnamara@cpan.org
#
# Documentation after __END__
#

# perltidy with the following options: -mbl=2 -pt=0 -nola

use 5.010000;
use strict;
use warnings;
use Carp;
use Excel::Writer::XLSX::Package::XMLwriter;

our @ISA     = qw(Excel::Writer::XLSX::Package::XMLwriter);
our $VERSION = '0.33';


###############################################################################
#
# Public and private API methods.
#
###############################################################################


###############################################################################
#
# new()
#
# Constructor.
#
sub new {

    my $class = shift;
    my $self  = Excel::Writer::XLSX::Package::XMLwriter->new();

    $self->{_writer}       = undef;
    $self->{_strings}      = [];
    $self->{_string_count} = 0;
    $self->{_unique_count} = 0;

    bless $self, $class;

    return $self;
}


###############################################################################
#
# _assemble_xml_file()
#
# Assemble and write the XML file.
#
sub _assemble_xml_file {

    my $self = shift;

    return unless $self->{_writer};

    $self->_write_xml_declaration;

    # Write the sst table.
    $self->_write_sst( $self->{_string_count}, $self->{_unique_count} );

    # Write the sst strings.
    $self->_write_sst_strings();

    # Close the sst tag.
    $self->{_writer}->endTag( 'sst' );

    # Close the XM writer object and filehandle.
    $self->{_writer}->end();
    $self->{_writer}->getOutput()->close();
}


###############################################################################
#
# _set_string_count()
#
# Set the total sst string count.
#
sub _set_string_count {

    my $self = shift;

    $self->{_string_count} = shift;
}


###############################################################################
#
# _set_unique_count()
#
# Set the total of unique sst strings.
#
sub _set_unique_count {

    my $self = shift;

    $self->{_unique_count} = shift;
}


###############################################################################
#
# _add_strings()
#
# Add the array ref of strings to be written.
#
sub _add_strings {

    my $self = shift;

    $self->{_strings} = shift;
}


###############################################################################
#
# Internal methods.
#
###############################################################################


###############################################################################
#
# XML writing methods.
#
###############################################################################


##############################################################################
#
# _write_sst()
#
# Write the <sst> element.
#
sub _write_sst {

    my $self         = shift;
    my $count        = shift;
    my $unique_count = shift;
    my $schema       = 'http://schemas.openxmlformats.org';
    my $xmlns        = $schema . '/spreadsheetml/2006/main';

    my @attributes = (
        'xmlns'       => $xmlns,
        'count'       => $count,
        'uniqueCount' => $unique_count,
    );

    $self->{_writer}->startTag( 'sst', @attributes );
}


###############################################################################
#
# _write_sst_strings()
#
# Write the sst string elements.
#
sub _write_sst_strings {

    my $self = shift;

    for my $string ( @{ $self->{_strings} } ) {
        $self->_write_si( $string );
    }
}


##############################################################################
#
# _write_si()
#
# Write the <si> element.
#
sub _write_si {

    my $self       = shift;
    my $string     = shift;
    my @attributes = ();

    if ( $string =~ /^\s/ || $string =~ /\s$/ ) {
        push @attributes, ( 'xml:space' => 'preserve' );
    }

    $self->{_writer}->startTag( 'si' );

    # Write any rich strings without further tags.
    if ( $string =~ m{^<r>} && $string =~ m{</r>$} ) {
        my $fh = $self->{_writer}->getOutput();
        print $fh $string;
    }
    else {
        $self->{_writer}->dataElement( 't', $string, @attributes );
    }

    $self->{_writer}->endTag( 'si' );
}


1;


__END__

=pod

=head1 NAME

SharedStrings - A class for writing the Excel XLSX sharedStrings.xml file.

=head1 SYNOPSIS

See the documentation for L<Excel::Writer::XLSX>.

=head1 DESCRIPTION

This module is used in conjunction with L<Excel::Writer::XLSX>.

=head1 AUTHOR

John McNamara jmcnamara@cpan.org

=head1 COPYRIGHT

� MM-MMXI, John McNamara.

All Rights Reserved. This module is free software. It may be used, redistributed and/or modified under the same terms as Perl itself.

=head1 LICENSE

Either the Perl Artistic Licence L<http://dev.perl.org/licenses/artistic.html> or the GPL L<http://www.opensource.org/licenses/gpl-license.php>.

=head1 DISCLAIMER OF WARRANTY

See the documentation for L<Excel::Writer::XLSX>.

=cut
