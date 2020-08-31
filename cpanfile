#!perl

requires 'Class::Inline';
requires 'OptArgs2';
requires 'Types::Standard';

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

feature 'sqltidy-sqlite', 'SQLite database support' => sub {
    recommends 'DBIx::ThinSQL';
    recommends 'DBD::SQLite';
};

# vim: ft=perl
