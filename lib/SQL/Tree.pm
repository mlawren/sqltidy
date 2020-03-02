package SQL::Tree;
use strict;
use warnings;
use constant DEBUG => 0;
use Data::Dumper;

our $VERSION = '0.2';

sub new {
    my ( $class, $sql ) = @_;
    my $r = { sql => $sql, };
    return bless( $r, $class );
}

sub pretty {
    my ($self) = @_;

    my $branches = $self->_sql_to_tree( $self->{sql} );

    if (DEBUG) {
        print Dumper($branches);
    }

    my $output = '';
    for my $branch (@$branches) {
        $output .= $self->_get_formatted_branch( 0, $branch );
    }

    return $output;
}

sub _get_formatted_branch {

    # Takes a tree, and turns it into nice looking SQL
    # Lots of special cases around, for taste

    my ( $self, $indent_level, $branch ) = @_;

    my $indent     = "    " x $indent_level;
    my $sub_indent = "    " x ( $indent_level + 1 );

    if ( ref $branch eq 'HASH' ) {

        # this is the major keyword of the branch
        my $name = $branch->{'name'};

        # First line
        my $first_line = $name;
        my $skip_space = 0;

        for my $word ( @{ $branch->{'top'} // [] } ) {
            if ( ref($word) eq 'ARRAY' ) {
                $first_line .= "\n";
                for my $line_branch (@$word) {
                    $first_line .=
                      $self->_get_formatted_branch( $indent_level + 1,
                        $line_branch );
                }

          # After the indented array, the next character will be a close-parens.
          # Don't put a space before it.
                $skip_space = 1;
            }
            else {
                $first_line .= $skip_space ? '' : ' ';
                $first_line .= $word;
            }
        }

        # Indented subsequent lines
        my $next_lines = '';
        for my $line ( @{ $branch->{'bottom'} // [] } ) {
            if ( ref($line) eq 'ARRAY' ) {
                for my $line_branch (@$line) {
                    $next_lines .=
                      $self->_get_formatted_branch( $indent_level + 2,
                        $line_branch );
                }
            }
            else {
                $next_lines .= "$sub_indent$line\n";
            }
        }

        my $ret = '';
        $ret .= "$indent$first_line\n";
        $ret .= $next_lines;

        return $ret;
    }
    elsif ( ref $branch eq 'ARRAY' ) {

        # It's a subquery
        my $next_lines = '';
        for my $line (@$branch) {
            $next_lines .=
              $self->_get_formatted_branch( $indent_level + 1, $line );
        }

        return $next_lines;

    }
    else {
        # it's a set of unparsed elements without a major or subquery
        return "$indent$branch\n";
    }

}

sub _sql_to_tree {

    # This function breaks things up into chunks that make sense
    # from a token perspective, but not one that knows about keywords.
    # We have to restructure the chunks a bit to be keyword specific,
    # so that's part of what we do in each major process_$function.

    my ( $self, $sql ) = @_;

    # Find major markers: select, from, where, group, parens
    my @majors         = ();
    my $current_major  = undef;
    my @current_minors = ();

# For each token, figure out what major it belongs to.  Process majors as we go.
    for my $token ( $self->_get_tokens($sql), '__END_OF_SQL' ) {
        print "My token is |$token|\n" if DEBUG;

        my $sub_name = $self->_token_to_sub_name($token);

        # If we've reached the start of a new major
        if ( $self->can($sub_name) or ( $token eq '__END_OF_SQL' ) ) {

            if ($current_major) {
                my $function = $self->_token_to_sub_name($current_major);
                push( @majors,
                    $self->$function( $current_major, @current_minors ) );
            }
            elsif ( @current_minors > 0 ) {

         # If there are no majors, its just a list.  If there are no subselects,
         # and there are fewer than 4, then merge them all into a single line.
         # Otherwise, each element get its own line.  We do it here, instead of
         # in a formatter, because the formatters aren't aware of the concept of
         # "major" and so can't differentiate when to do the inline style.

                # TBD: Check for 3-items-or-less and no-subqueries

                # Otherwise, one per line
                for my $minor (@current_minors) {
                    if ( ref($minor) eq 'ARRAY' ) {

                        # if its a subselect, wrap it in parens
                        push( @majors, '(', $minor, ')' );
                    }
                    else {
                        # If its comma, merge it back into a previous one
                        if ( $minor eq ',' && @majors ) {
                            $majors[-1] .= $minor;
                        }
                        else {
                            push( @majors, $minor );
                        }
                    }
                }
            }

            $current_major  = $token;
            @current_minors = ();
        }
        else {
  # if we run into ( ... ), it may have subqueries in it, so parse it as a tree
  # the parenthesis disappear here when the subquery is converted to a tree, but
  # its always added back in when we reforge the list or conditionals
            if ( $token =~ /^\((.*)\)$/s ) {
                my $new_token = $self->_sql_to_tree($1);

                # major blocks will come out as hashes
                my $has_majors   = grep { ref($_) eq 'HASH' } @$new_token;
                my $has_subquery = grep { ref($_) eq 'ARRAY' } @$new_token;

                if ( $has_majors or $has_subquery ) {
                    $token = $new_token;
                }
                else {
              # Special case for functions that don't have majors inside of them
                    $token = '(' . join( ' ', @$new_token ) . ')';
                }
            }

            push( @current_minors, $token );
        }

    }

    return \@majors;
}

sub process_select          { list_formatter(@_) }
sub process_select_distinct { list_formatter(@_) }
sub process_from            { list_formatter(@_) }
sub process_delete_from     { list_formatter(@_) }
sub process_group_by        { list_formatter(@_) }
sub process_order_by        { list_formatter(@_) }
sub process_insert_into     { list_formatter(@_) }
sub process_values          { list_formatter(@_) }
sub process_join            { join_formatter(@_) }
sub process_left_join       { join_formatter(@_) }
sub process_right_join      { join_formatter(@_) }
sub process_inner_join      { join_formatter(@_) }
sub process_outer_join      { join_formatter(@_) }
sub process_full_join       { join_formatter(@_) }
sub process_where           { where_formatter(@_) }
sub process_having          { where_formatter(@_) }
sub process_update          { update_formatter(@_) }
sub process_union           { identity_formatter(@_) }
sub process_union_all       { identity_formatter(@_) }
sub process_limit           { identity_formatter(@_) }
sub process_SLASH           { identity_formatter(@_) }

##################################################

# Standard sort of tokenization.  The only special thing we
# do here is that parentheticals are one giant token.  The
# tree-maker knows it needs to break them up when it comes to
# them, but this allows our tokenization logic to be extra simple.

sub _get_tokens {
    my ( $self, $sql ) = @_;

    # each word is its own token, except if its in a string or comment
    my @tokens            = ();
    my $current_token     = '';
    my $is_in_string      = 0;
    my $is_in_comment     = 0;
    my $parenthesis_depth = 0;
    my $escaped           = 0;

    for my $c ( split( //, $sql ) ) {

        if ($escaped) {
            $current_token .= $c;
            $escaped = 0;
        }
        elsif ( $c eq "\\" ) {
            $current_token .= $c;
            $escaped = 1;
        }
        elsif ( $parenthesis_depth > 0 ) {
            $current_token .= $c;
            if ( $c eq '(' ) {
                $parenthesis_depth++;
            }
            elsif ( $c eq ')' ) {
                $parenthesis_depth--;
                if ( $parenthesis_depth == 0 ) {
                    push( @tokens, $current_token );
                    $current_token = '';
                }
            }
        }
        elsif ($is_in_string) {
            $current_token .= $c;
            if ( $c eq $is_in_string ) {
                $is_in_string = 0;
            }
        }
        elsif ($is_in_comment) {
            if ( $c eq "\n" ) {
                $is_in_comment = 0;
                push( @tokens, $current_token );
                $current_token = '';
            }
            else {
                $current_token .= $c;
            }
        }
        elsif ( $c =~ /^\s$/s ) {
            if ( $self->_is_multiword_token( \@tokens, $current_token ) ) {
                $tokens[-1] .= " $current_token";
            }
            elsif ( $current_token ne '' ) {
                push( @tokens, $current_token );
            }
            $current_token = '';
        }
        else {
            # If we're making a function call, split the call token
            # from the parameters, so that the parameters token is
            # parsed properly.  Same rule applies to equals, so that
            # there are no equals inside a token

            if ( $current_token ne '' && $c =~ /[(=]/ ) {
                push( @tokens, $current_token );
                $current_token = '';
            }

            $current_token .= $c;

            # No commas in the middle of tokens.  If its a comma
            # or eq, then new token.
            if ( $current_token ne '' && $c =~ /[,=]/ ) {
                push( @tokens, $current_token );
                $current_token = '';
            }

            if ( $current_token eq '--' ) {
                $is_in_comment = 1;
            }
            elsif ( $current_token =~ /^(["'])$/ ) {
                $is_in_string = $1;
            }
            elsif ( $c eq '(' ) {
                $parenthesis_depth = 1;
            }
        }
    }

    if ( $current_token ne '' ) {
        push( @tokens, $current_token );
    }

    return @tokens;
}

sub _token_to_sub_name {
    my ( $self, $token ) = @_;

    $token = lc $token;
    if ($token eq '/') {
        $token = 'SLASH';
    }

    my $subroutine = "process_$token";
    $subroutine =~ s/\s/_/g;

    return $subroutine;
}

# Since SQL isn't a postfix language, we need to be able
# to remember what we've seen before, so we can identify
# multi-word keywords like "left join".

sub _is_multiword_token {
    my ( $self, $all_tokens, $second ) = @_;

    return 0 unless @$all_tokens;
    my $first = $all_tokens->[-1];
    $first = lc $first;
    $second = lc $second;

    my @multiword_tokens = (
        "select distinct",
        "inner join",
        "left join",
        "right join",
        "outer join",
        "full join",
        "group by",
        "order by",
        "union all",
        "insert into",
    );

    return scalar( grep { "$first $second" eq $_ } @multiword_tokens );
}

##################################################

# These functions take lists of tokens and reforge them
# into tokens meant for display

sub list_formatter {
    my ( $self, $major, @minors ) = @_;
    my ( $top, $bottom );

    # Reforge so tokens are separated by commas
    @minors = $self->_reforge_list(@minors);

    # If there's only one column, inline it
    if ( @minors == 1 ) {
        push( @$top, @minors );
    }

# Special handling for when there's just one subselect in the list, and no other values
    elsif ( @minors == 3 && $minors[0] eq '(' && $minors[2] eq ')' ) {
        push( @$top, @minors );
    }

    # Otherwise, give each column its own line
    else {
        $bottom = [@minors];
    }

    return +{
        name   => $major,
        top    => $top,
        bottom => $bottom,
    };
}

sub update_formatter {
    my ( $self, $major, @minors ) = @_;
    my ( $top, $bottom );

    $top = [ shift @minors, shift @minors ];

    # Reforge so tokens are separated by commas
    @minors = $self->_reforge_list(@minors);

    # If there's only one column, inline it
    if ( @minors == 1 ) {
        push( @$top, @minors );
    }

    # Otherwise, give each column its own line
    else {
        $bottom = [@minors];
    }

    return +{
        name   => $major,
        top    => $top,
        bottom => $bottom,
    };
}

sub join_formatter {
    my ( $self, $major, @minors ) = @_;
    my ( $top, $bottom ) = ( [], [] );

    my ( $top_array, $new_minors ) = $self->_reforge_on(@minors);

    $top    = [@$top_array];
    $bottom = [ $self->_reforge_conditionals(@$new_minors) ];

    return +{
        name   => $major,
        top    => $top,
        bottom => $bottom,
    };
}

sub where_formatter {
    my ( $self, $major, @minors ) = @_;
    my ( $top, $bottom ) = ( [], [] );

    @minors = $self->_reforge_conditionals(@minors);

    if ( @minors == 1 ) {
        $top = [@minors];
    }
    else {
        $bottom = [@minors];
    }

    return +{
        name   => $major,
        top    => $top,
        bottom => $bottom,
    };
}

sub identity_formatter {
    my ( $self, $major, @minors ) = @_;

    return +{
        name   => $major,
        top    => [@minors],
        bottom => [],
    };
}

sub _reforge_conditionals {
    my ( $self, @minors ) = @_;

    my @new_minors     = ();
    my @current_tokens = ();

    for my $token ( @minors, '__END_OF_SQL' ) {

        if ( ref($token) eq 'ARRAY' ) {
            push( @new_minors, join( " ", @current_tokens, '(' ) );
            push( @new_minors, $token );
            @current_tokens = (')');
        }
        elsif (lc($token) =~ /^(?:and|or)$/
            || $token =~ /^--/
            || $token eq '__END_OF_SQL' )
        {
            push( @current_tokens, $token ) unless $token eq '__END_OF_SQL';
            push( @new_minors,     join( " ", @current_tokens ) );
            @current_tokens = ();
        }
        else {
            push( @current_tokens, $token );
        }
    }

    return @new_minors;
}

sub _reforge_on {
    my ( $self, @minors ) = @_;

    my @new_top    = ();
    my @new_minors = ();
    my $seen_on    = 0;

    for my $token (@minors) {
        push( @new_minors, $token );
        if ( $token eq 'on' ) {
            @new_top    = $self->_reforge_list(@new_minors);
            @new_minors = ();
        }
    }

    return ( \@new_top, \@new_minors );
}

sub _reforge_list {
    my ( $self, @minors ) = @_;

    # the purpose of the reforge is to comma delimited lines, so we reforge the
    # smaller tokens into the larger tokens used by the display functions.  We
    # must leave things tokenized so we don't reforge on commas inside strings.

    my @new_minors     = ();
    my @current_tokens = ();
    for my $token ( @minors, '__END_OF_LIST' ) {

        if ( $token eq ',' && @current_tokens > 0 ) {
            $current_tokens[-1] .= ",";
            push( @new_minors, join( " ", @current_tokens ) );
            @current_tokens = ();
        }
        elsif ( ref($token) eq 'ARRAY' ) {
            push( @new_minors, join( " ", @current_tokens, '(' ) );
            push( @new_minors, $token );
            @current_tokens = (')');
        }
        elsif ( $token =~ /,$/ || $token =~ /^--/ || $token eq '__END_OF_LIST' )
        {
            push( @current_tokens, $token ) unless $token eq '__END_OF_LIST';
            push( @new_minors,     join( " ", @current_tokens ) );
            @current_tokens = ();
        }
        else {
            push( @current_tokens, $token );
        }
    }

    return @new_minors;
}

1;

__END__

=head1 NAME

SQL::Tree - parse and prettify SQL text

=head1 SYNOPSIS

    use SQL::Tree;
    my $tree   = new SQL::Tree($sql);
    my $output = $tree->pretty();

=head1 DESCRIPTION

Parses SQL text and reformats it nicely indented.

=head1 AUTHOR

Ryan Kosai <github@ryankosai.com>

=head1 COPYRIGHT AND LICENSE

Copyright 2011 Ryan Kosai <github@ryankosai.com>
