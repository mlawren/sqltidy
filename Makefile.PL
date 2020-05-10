#!/usr/bin/env perl
use strict;
use warnings;
use 5.010;
use ExtUtils::MakeMaker::CPANfile;

WriteMakefile(
    NAME         => 'SQL::Tidy',
    ABSTRACT     => 'Parse and prettify SQL text',
    AUTHOR       => 'Mark Lawrence <nomad@null.net>',
    VERSION_FROM => 'lib/SQL/Tidy.pm',
    LICENSE      => 'gpl_3',
    EXE_FILES    => ['bin/sqltidy'],
    test         => {
        RECURSIVE_TEST_FILES => 1,
    },
    META_MERGE => {
        'meta-spec' => { version => 2 },
        resources   => {
            bugtracker =>
              { web => 'https//github.com/mlawren/p5-SQL-Tidy/issues', },
            homepage   => 'https//github.com/mlawren/p5-SQL-Tidy',
            repository => {
                type => 'git',
                url  => 'ssh://git@github.com/mlawren/p5-SQL-Tidy.git',
                web  => 'https//github.com/mlawren/p5-SQL-Tidy',
            }
        },
    },
);
