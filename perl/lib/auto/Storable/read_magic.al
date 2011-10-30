# NOTE: Derived from blib\lib\Storable.pm.
# Changes made here will be lost when autosplit is run again.
# See AutoSplit.pm.
package Storable;

#line 131 "blib\lib\Storable.pm (autosplit into blib\lib\auto\Storable\read_magic.al)"
sub read_magic {
    my($buf, $file) = @_;
    my %info;

    my $buflen = length($buf);
    my $magic;
    if ($buf =~ s/^(pst0|perl-store)//) {
	$magic = $1;
	$info{file} = $file || 1;
    }
    else {
	return undef if $file;
	$magic = "";
    }

    return undef unless length($buf);

    my $net_order;
    if ($magic eq "perl-store" && ord(substr($buf, 0, 1)) > 1) {
	$info{version} = -1;
	$net_order = 0;
    }
    else {
	$buf =~ s/(.)//s;
	my $major = (ord $1) >> 1;
	return undef if $major > 4; # sanity (assuming we never go that high)
	$info{major} = $major;
	$net_order = (ord $1) & 0x01;
	if ($major > 1) {
	    return undef unless $buf =~ s/(.)//s;
	    my $minor = ord $1;
	    $info{minor} = $minor;
	    $info{version} = "$major.$minor";
	    $info{version_nv} = sprintf "%d.%03d", $major, $minor;
	}
	else {
	    $info{version} = $major;
	}
    }
    $info{version_nv} ||= $info{version};
    $info{netorder} = $net_order;

    unless ($net_order) {
	return undef unless $buf =~ s/(.)//s;
	my $len = ord $1;
	return undef unless length($buf) >= $len;
	return undef unless $len == 4 || $len == 8;  # sanity
	@info{qw(byteorder intsize longsize ptrsize)}
	    = unpack "a${len}CCC", $buf;
	(substr $buf, 0, $len + 3) = '';
	if ($info{version_nv} >= 2.002) {
	    return undef unless $buf =~ s/(.)//s;
	    $info{nvsize} = ord $1;
	}
    }
    $info{hdrsize} = $buflen - length($buf);

    return \%info;
}

# end of Storable::read_magic
1;
