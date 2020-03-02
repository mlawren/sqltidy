#!/usr/bin/perl
use strict;
use warnings;
use FindBin qw/$Bin/;
use Path::Tiny;
use SQL::Tree;
use Test::More;

path( $Bin, 'data' )->visit(
    \&check_tidy,
    {
        recurse         => 1,
        follow_symlinks => 0,
    }
);

sub check_tidy {
    my $file = shift;
    return unless -f $file and $file =~ m/\.sql$/;

    my $sql  = $file->slurp_utf8;
    my $want = path( $file . '.tdy.canonical' )->slurp_utf8;
    my $tree = SQL::Tree->new($sql);
    my $tidy = $tree->pretty;

    is $tidy, $want, $file->basename;
}

done_testing();
