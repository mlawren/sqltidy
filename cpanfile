#!perl

on configure => sub {
    requires 'ExtUtils::MakeMaker::CPANfile';
};

on develop => sub {
    requires 'App::githook::perltidy';
};

on test => sub {
    requires 'FindBin';
    requires 'Path::Tiny';
    requires 'Test::More';
};

# vim: ft=perl
