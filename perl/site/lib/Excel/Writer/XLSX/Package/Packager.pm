package Excel::Writer::XLSX::Package::Packager;

###############################################################################
#
# Packager - A class for creating the Excel XLSX package.
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
use Exporter;
use Carp;
use File::Copy;
use Excel::Writer::XLSX::Package::App;
use Excel::Writer::XLSX::Package::Comments;
use Excel::Writer::XLSX::Package::ContentTypes;
use Excel::Writer::XLSX::Package::Core;
use Excel::Writer::XLSX::Package::Relationships;
use Excel::Writer::XLSX::Package::SharedStrings;
use Excel::Writer::XLSX::Package::Styles;
use Excel::Writer::XLSX::Package::Theme;
use Excel::Writer::XLSX::Package::VML;

our @ISA     = qw(Exporter);
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

    my $self = {
        _package_dir      => '',
        _workbook         => undef,
        _sheet_names      => [],
        _worksheet_count  => 0,
        _chartsheet_count => 0,
        _chart_count      => 0,
        _drawing_count    => 0,
        _named_ranges     => [],
    };


    bless $self, $class;

    return $self;
}


###############################################################################
#
# _set_package_dir()
#
# Set the XLSX OPC package directory.
#
sub _set_package_dir {

    my $self = shift;

    $self->{_package_dir} = shift;
}


###############################################################################
#
# _add_workbook()
#
# Add the Excel::Writer::XLSX::Workbook object to the package.
#
sub _add_workbook {

    my $self        = shift;
    my $workbook    = shift;
    my @sheet_names = @{ $workbook->{_sheetnames} };

    $self->{_workbook}          = $workbook;
    $self->{_sheet_names}       = \@sheet_names;
    $self->{_chart_count}       = scalar @{ $workbook->{_charts} };
    $self->{_drawing_count}     = scalar @{ $workbook->{_drawings} };
    $self->{_num_comment_files} = $workbook->{_num_comment_files};
    $self->{_named_ranges}      = $workbook->{_named_ranges};

    for my $worksheet ( @{ $self->{_workbook}->{_worksheets} } ) {
        if ( $worksheet->{_is_chartsheet} ) {
            $self->{_chartsheet_count}++;
        }
        else {
            $self->{_worksheet_count}++;
        }
    }
}


###############################################################################
#
# _create_package()
#
# Write the xml files that make up the XLXS OPC package.
#
sub _create_package {

    my $self = shift;

    $self->_write_worksheet_files();
    $self->_write_chartsheet_files();
    $self->_write_workbook_file();
    $self->_write_chart_files();
    $self->_write_drawing_files();
    $self->_write_vml_files();
    $self->_write_comment_files();
    $self->_write_shared_strings_file();
    $self->_write_app_file();
    $self->_write_core_file();
    $self->_write_content_types_file();
    $self->_write_styles_file();
    $self->_write_theme_file();
    $self->_write_root_rels_file();
    $self->_write_workbook_rels_file();
    $self->_write_worksheet_rels_files();
    $self->_write_chartsheet_rels_files();
    $self->_write_drawing_rels_files();
    $self->_add_image_files();
}


###############################################################################
#
# _write_workbook_file()
#
# Write the workbook.xml file.
#
sub _write_workbook_file {

    my $self     = shift;
    my $dir      = $self->{_package_dir};
    my $workbook = $self->{_workbook};

    mkdir $dir . '/xl';

    $workbook->_set_xml_writer( $dir . '/xl/workbook.xml' );
    $workbook->_assemble_xml_file();
}


###############################################################################
#
# _write_worksheet_files()
#
# Write the worksheet files.
#
sub _write_worksheet_files {

    my $self = shift;
    my $dir  = $self->{_package_dir};

    mkdir $dir . '/xl';
    mkdir $dir . '/xl/worksheets';

    my $index = 1;
    for my $worksheet ( @{ $self->{_workbook}->{_worksheets} } ) {
        next if $worksheet->{_is_chartsheet};

        $worksheet->_set_xml_writer(
            $dir . '/xl/worksheets/sheet' . $index++ . '.xml' );
        $worksheet->_assemble_xml_file();

    }
}


###############################################################################
#
# _write_chartsheet_files()
#
# Write the chartsheet files.
#
sub _write_chartsheet_files {

    my $self = shift;
    my $dir  = $self->{_package_dir};

    mkdir $dir . '/xl';
    mkdir $dir . '/xl/chartsheets';

    my $index = 1;
    for my $worksheet ( @{ $self->{_workbook}->{_worksheets} } ) {
        next unless $worksheet->{_is_chartsheet};

        $worksheet->_set_xml_writer(
            $dir . '/xl/chartsheets/sheet' . $index++ . '.xml' );
        $worksheet->_assemble_xml_file();

    }
}


###############################################################################
#
# _write_chart_files()
#
# Write the chart files.
#
sub _write_chart_files {

    my $self = shift;
    my $dir  = $self->{_package_dir};

    return unless @{ $self->{_workbook}->{_charts} };

    mkdir $dir . '/xl';
    mkdir $dir . '/xl/charts';

    my $index = 1;
    for my $chart ( @{ $self->{_workbook}->{_charts} } ) {
        $chart->_set_xml_writer(
            $dir . '/xl/charts/chart' . $index++ . '.xml' );
        $chart->_assemble_xml_file();

    }
}


###############################################################################
#
# _write_drawing_files()
#
# Write the drawing files.
#
sub _write_drawing_files {

    my $self = shift;
    my $dir  = $self->{_package_dir};

    return unless $self->{_drawing_count};

    mkdir $dir . '/xl';
    mkdir $dir . '/xl/drawings';

    my $index = 1;
    for my $drawing ( @{ $self->{_workbook}->{_drawings} } ) {
        $drawing->_set_xml_writer(
            $dir . '/xl/drawings/drawing' . $index++ . '.xml' );
        $drawing->_assemble_xml_file();
    }
}


###############################################################################
#
# _write_vml_files()
#
# Write the comment VML files.
#
sub _write_vml_files {

    my $self = shift;
    my $dir  = $self->{_package_dir};

    mkdir $dir . '/xl';
    mkdir $dir . '/xl/drawings';

    my $index   = 1;
    for my $worksheet ( @{ $self->{_workbook}->{_worksheets} } ) {
        next unless $worksheet->{_has_comments};

        my $vml = Excel::Writer::XLSX::Package::VML->new();

        $vml->_set_xml_writer(
            $dir . '/xl/drawings/vmlDrawing' . $index++ . '.vml' );
        $vml->_assemble_xml_file(
            $worksheet->{_vml_data_id},
            $worksheet->{_vml_shape_id},
            $worksheet->{_comments_array}
        );
    }
}


###############################################################################
#
# _write_comment_files()
#
# Write the comment files.
#
sub _write_comment_files {

    my $self = shift;
    my $dir  = $self->{_package_dir};

    mkdir $dir . '/xl';
    mkdir $dir . '/xl/drawings';

    my $index = 1;
    for my $worksheet ( @{ $self->{_workbook}->{_worksheets} } ) {
        next unless $worksheet->{_has_comments};

        my $comment = Excel::Writer::XLSX::Package::Comments->new();

        $comment->_set_xml_writer( $dir . '/xl/comments' . $index++ . '.xml' );
        $comment->_assemble_xml_file( $worksheet->{_comments_array} );
    }
}


###############################################################################
#
# _write_shared_strings_file()
#
# Write the sharedStrings.xml file.
#
sub _write_shared_strings_file {

    my $self = shift;
    my $dir  = $self->{_package_dir};
    my $sst  = Excel::Writer::XLSX::Package::SharedStrings->new();

    mkdir $dir . '/xl';

    my $total    = $self->{_workbook}->{_str_total};
    my $unique   = $self->{_workbook}->{_str_unique};
    my $sst_data = $self->{_workbook}->{_str_array};

    return unless $total > 0;

    $sst->_set_string_count( $total );
    $sst->_set_unique_count( $unique );
    $sst->_add_strings( $sst_data );

    $sst->_set_xml_writer( $dir . '/xl/sharedStrings.xml' );
    $sst->_assemble_xml_file();
}


###############################################################################
#
# _write_app_file()
#
# Write the app.xml file.
#
sub _write_app_file {

    my $self       = shift;
    my $dir        = $self->{_package_dir};
    my $properties = $self->{_workbook}->{_doc_properties};
    my $app        = Excel::Writer::XLSX::Package::App->new();

    mkdir $dir . '/docProps';

    # Add the Worksheet heading pairs.
    $app->_add_heading_pair( [ 'Worksheets', $self->{_worksheet_count} ] );

    # Add the Chartsheet heading pairs.
    $app->_add_heading_pair( [ 'Charts', $self->{_chartsheet_count} ] );

    # Add the Worksheet parts.
    for my $worksheet ( @{ $self->{_workbook}->{_worksheets} } ) {
        next if $worksheet->{_is_chartsheet};
        $app->_add_part_name( $worksheet->get_name() );
    }

    # Add the Chartsheet parts.
    for my $worksheet ( @{ $self->{_workbook}->{_worksheets} } ) {
        next unless $worksheet->{_is_chartsheet};
        $app->_add_part_name( $worksheet->get_name() );
    }

    # Add the Named Range heading pairs.
    if ( my $range_count = scalar @{ $self->{_named_ranges} } ) {
        $app->_add_heading_pair( [ 'Named Ranges', $range_count ] );
    }

    # Add the Named Ranges parts.
    for my $named_range ( @{ $self->{_named_ranges} } ) {
        $app->_add_part_name( $named_range );
    }

    $app->_set_properties( $properties );

    $app->_set_xml_writer( $dir . '/docProps/app.xml' );
    $app->_assemble_xml_file();
}


###############################################################################
#
# _write_core_file()
#
# Write the core.xml file.
#
sub _write_core_file {

    my $self       = shift;
    my $dir        = $self->{_package_dir};
    my $properties = $self->{_workbook}->{_doc_properties};
    my $core       = Excel::Writer::XLSX::Package::Core->new();

    mkdir $dir . '/docProps';

    $core->_set_properties( $properties );
    $core->_set_xml_writer( $dir . '/docProps/core.xml' );
    $core->_assemble_xml_file();
}


###############################################################################
#
# _write_content_types_file()
#
# Write the ContentTypes.xml file.
#
sub _write_content_types_file {

    my $self    = shift;
    my $dir     = $self->{_package_dir};
    my $content = Excel::Writer::XLSX::Package::ContentTypes->new();

    $content->_add_image_types( %{ $self->{_workbook}->{_image_types} } );

    my $worksheet_index  = 1;
    my $chartsheet_index = 1;
    for my $worksheet ( @{ $self->{_workbook}->{_worksheets} } ) {
        if ( $worksheet->{_is_chartsheet} ) {
            $content->_add_chartsheet_name( 'sheet' . $chartsheet_index++ );
        }
        else {
            $content->_add_worksheet_name( 'sheet' . $worksheet_index++ );
        }
    }

    for my $i ( 1 .. $self->{_chart_count} ) {
        $content->_add_chart_name( 'chart' . $i );
    }

    for my $i ( 1 .. $self->{_drawing_count} ) {
        $content->_add_drawing_name( 'drawing' . $i );
    }

    if ( $self->{_num_comment_files} ) {
        $content->_add_vml_name();
    }

    for my $i ( 1 .. $self->{_num_comment_files} ) {
        $content->_add_comment_name( 'comments' . $i );
    }

    # Add the sharedString rel if there is string data in the workbook.
    if ( $self->{_workbook}->{_str_total} ) {
        $content->_add_shared_strings();
    }

    $content->_set_xml_writer( $dir . '/[Content_Types].xml' );
    $content->_assemble_xml_file();
}


###############################################################################
#
# _write_styles_file()
#
# Write the style xml file.
#
sub _write_styles_file {

    my $self             = shift;
    my $dir              = $self->{_package_dir};
    my $xf_formats       = $self->{_workbook}->{_xf_formats};
    my $palette          = $self->{_workbook}->{_palette};
    my $font_count       = $self->{_workbook}->{_font_count};
    my $num_format_count = $self->{_workbook}->{_num_format_count};
    my $border_count     = $self->{_workbook}->{_border_count};
    my $fill_count       = $self->{_workbook}->{_fill_count};
    my $custom_colors    = $self->{_workbook}->{_custom_colors};
    my $dxf_formats      = $self->{_workbook}->{_dxf_formats};

    my $rels = Excel::Writer::XLSX::Package::Styles->new();

    mkdir $dir . '/xl';

    $rels->_set_style_properties(
        $xf_formats,
        $palette,
        $font_count,
        $num_format_count,
        $border_count,
        $fill_count,
        $custom_colors,
        $dxf_formats,

    );

    $rels->_set_xml_writer( $dir . '/xl/styles.xml' );
    $rels->_assemble_xml_file();
}


###############################################################################
#
# _write_theme_file()
#
# Write the style xml file.
#
sub _write_theme_file {

    my $self = shift;
    my $dir  = $self->{_package_dir};
    my $rels = Excel::Writer::XLSX::Package::Theme->new();

    mkdir $dir . '/xl';
    mkdir $dir . '/xl/theme';

    $rels->_set_xml_writer( $dir . '/xl/theme/theme1.xml' );
    $rels->_assemble_xml_file();
}


###############################################################################
#
# _write_root_rels_file()
#
# Write the _rels/.rels xml file.
#
sub _write_root_rels_file {

    my $self = shift;
    my $dir  = $self->{_package_dir};
    my $rels = Excel::Writer::XLSX::Package::Relationships->new();

    mkdir $dir . '/_rels';

    $rels->_add_document_relationship( '/officeDocument', 'xl/workbook.xml' );
    $rels->_add_package_relationship( '/metadata/core-properties',
        'docProps/core' );
    $rels->_add_document_relationship( '/extended-properties', 'docProps/app.xml' );

    $rels->_set_xml_writer( $dir . '/_rels/.rels' );
    $rels->_assemble_xml_file();
}


###############################################################################
#
# _write_workbook_rels_file()
#
# Write the _rels/.rels xml file.
#
sub _write_workbook_rels_file {

    my $self = shift;
    my $dir  = $self->{_package_dir};
    my $rels = Excel::Writer::XLSX::Package::Relationships->new();

    mkdir $dir . '/xl';
    mkdir $dir . '/xl/_rels';

    my $worksheet_index  = 1;
    my $chartsheet_index = 1;

    for my $worksheet ( @{ $self->{_workbook}->{_worksheets} } ) {
        if ( $worksheet->{_is_chartsheet} ) {
            $rels->_add_document_relationship( '/chartsheet',
                'chartsheets/sheet' . $chartsheet_index++ . '.xml' );
        }
        else {
            $rels->_add_document_relationship( '/worksheet',
                'worksheets/sheet' . $worksheet_index++. '.xml' );
        }
    }

    $rels->_add_document_relationship( '/theme',  'theme/theme1.xml' );
    $rels->_add_document_relationship( '/styles', 'styles.xml' );

    # Add the sharedString rel if there is string data in the workbook.
    if ( $self->{_workbook}->{_str_total} ) {
        $rels->_add_document_relationship( '/sharedStrings', 'sharedStrings.xml' );
    }

    $rels->_set_xml_writer( $dir . '/xl/_rels/workbook.xml.rels' );
    $rels->_assemble_xml_file();
}


###############################################################################
#
# _write_worksheet_rels_files()
#
# Write the worksheet .rels files for worksheets that contain links to external
# data such as hyperlinks or drawings.
#
sub _write_worksheet_rels_files {

    my $self              = shift;
    my $dir               = $self->{_package_dir};
    my $existing_rels_dir = 0;


    my $index = 0;
    for my $worksheet ( @{ $self->{_workbook}->{_worksheets} } ) {

        next if $worksheet->{_is_chartsheet};

        $index++;

        my @external_links = (
            @{ $worksheet->{_external_hyper_links} },
            @{ $worksheet->{_external_drawing_links} },
            @{ $worksheet->{_external_comment_links} },
        );

        next unless @external_links;

        # Create the worksheet .rels dir if required.
        if ( !$existing_rels_dir ) {
            mkdir $dir . '/xl';
            mkdir $dir . '/xl/worksheets';
            mkdir $dir . '/xl/worksheets/_rels';
            $existing_rels_dir = 1;
        }

        my $rels = Excel::Writer::XLSX::Package::Relationships->new();

        for my $link_data ( @external_links ) {
            $rels->_add_worksheet_relationship( @$link_data );
        }

        # Create the .rels file such as /xl/worksheets/_rels/sheet1.xml.rels.
        $rels->_set_xml_writer(
            $dir . '/xl/worksheets/_rels/sheet' . $index . '.xml.rels' );
        $rels->_assemble_xml_file();
    }
}


###############################################################################
#
# _write_chartsheet_rels_files()
#
# Write the chartsheet .rels files for links to drawing files.
#
sub _write_chartsheet_rels_files {

    my $self              = shift;
    my $dir               = $self->{_package_dir};
    my $existing_rels_dir = 0;


    my $index = 0;
    for my $worksheet ( @{ $self->{_workbook}->{_worksheets} } ) {

        next unless $worksheet->{_is_chartsheet};

        $index++;

        my @external_links = @{ $worksheet->{_external_drawing_links} };

        next unless @external_links;

        # Create the chartsheet .rels dir if required.
        if ( !$existing_rels_dir ) {
            mkdir $dir . '/xl';
            mkdir $dir . '/xl/chartsheets';
            mkdir $dir . '/xl/chartsheets/_rels';
            $existing_rels_dir = 1;
        }

        my $rels = Excel::Writer::XLSX::Package::Relationships->new();

        for my $link_data ( @external_links ) {
            $rels->_add_worksheet_relationship( @$link_data );
        }

        # Create the .rels file such as /xl/chartsheets/_rels/sheet1.xml.rels.
        $rels->_set_xml_writer(
            $dir . '/xl/chartsheets/_rels/sheet' . $index . '.xml.rels' );
        $rels->_assemble_xml_file();
    }
}


###############################################################################
#
# _write_drawing_rels_files()
#
# Write the drawing .rels files for worksheets that contain charts or drawings.
#
sub _write_drawing_rels_files {

    my $self              = shift;
    my $dir               = $self->{_package_dir};
    my $existing_rels_dir = 0;


    my $index = 0;
    for my $worksheet ( @{ $self->{_workbook}->{_worksheets} } ) {
        next unless @{ $worksheet->{_drawing_links} };
        $index++;

        # Create the drawing .rels dir if required.
        if ( !$existing_rels_dir ) {
            mkdir $dir . '/xl';
            mkdir $dir . '/xl/drawings';
            mkdir $dir . '/xl/drawings/_rels';
            $existing_rels_dir = 1;
        }

        my $rels = Excel::Writer::XLSX::Package::Relationships->new();

        for my $drawing_data ( @{ $worksheet->{_drawing_links} } ) {
            $rels->_add_document_relationship( @$drawing_data );
        }

        # Create the .rels file such as /xl/drawings/_rels/sheet1.xml.rels.
        $rels->_set_xml_writer(
            $dir . '/xl/drawings/_rels/drawing' . $index . '.xml.rels' );
        $rels->_assemble_xml_file();
    }
}


###############################################################################
#
# _add_image_files()
#
# Write the workbook.xml file.
#
sub _add_image_files {

    my $self     = shift;
    my $dir      = $self->{_package_dir};
    my $workbook = $self->{_workbook};
    my $index    = 1;

    mkdir $dir . '/xl';
    mkdir $dir . '/xl/media';

    for my $image ( @{ $workbook->{_images} } ) {
        my $filename  = $image->[0];
        my $extension = '.' . $image->[1];

        copy( $filename, $dir . '/xl/media/image' . $index++ . $extension );
    }
}


1;


__END__

=pod

=head1 NAME

Packager - A class for creating the Excel XLSX package.

=head1 SYNOPSIS

See the documentation for L<Excel::Writer::XLSX>.

=head1 DESCRIPTION

This module is used in conjunction with L<Excel::Writer::XLSX> to create an Excel XLSX container file.

From Wikipedia: I<The Open Packaging Conventions (OPC) is a container-file technology initially created by Microsoft to store a combination of XML and non-XML files that together form a single entity such as an Open XML Paper Specification (OpenXPS) document>. L<http://en.wikipedia.org/wiki/Open_Packaging_Conventions>.

At its simplest an Excel XLSX file contains the following elements:

     ____ [Content_Types].xml
    |
    |____ docProps
    | |____ app.xml
    | |____ core.xml
    |
    |____ xl
    | |____ workbook.xml
    | |____ worksheets
    | | |____ sheet1.xml
    | |
    | |____ styles.xml
    | |
    | |____ theme
    | | |____ theme1.xml
    | |
    | |_____rels
    |   |____ workbook.xml.rels
    |
    |_____rels
      |____ .rels


The C<Excel::Writer::XLSX::Package::Packager> class co-ordinates the classes that represent the elements of the package and writes them into the XLSX file.

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
