#!/usr/bin/perl
use strict;
use warnings;
use FindBin qw/$Bin/;
use Path::Tiny;
use SQL::Tidy;
use Test::More;

path( $Bin, 'data' )->visit(
    sub {
        my $file = shift;
        return unless -f $file and $file =~ m/\.sql$/;

        my $sql  = $file->slurp_utf8;
        my $tree = SQL::Tidy->new( uppercase => 0 );
        my $tidy = $tree->parse($sql);

        my $want = path( $file . '.tdy.canonical' )->slurp_utf8;
        is $tidy, $want, $file->basename;
    },
    {
        recurse         => 1,
        follow_symlinks => 0,
    }
);

done_testing();
