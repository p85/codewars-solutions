use strict;
package Solution;

sub summation{
    my ($p1) = @_;
    my @a = (0..$p1);
    my $result = 0;
    for(@a) {
        $result = $result + $_;
    }
    return $result;
}

1;