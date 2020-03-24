package SQL::Tree;
use strict;
use warnings;
use constant DEBUG => 1;
use Data::Dumper;

our $VERSION = '0.2';
our $INLINE  = {
    double_escape    => { default => 0, },
    indent           => { default => '    ', },
    preserve_leading => { default => 1, },
    uppercase        => { default => 1 },
};

##################################################

# Since SQL isn't a postfix language, we need to be able to remember
# what we've seen before, so we can identify multi-word keywords like
# "left join".

our @multiword_tokens = (
    'select distinct',
    'end as',
    'inner join',
    'left join',
    'right join',
    'outer join',
    'full join',
    'group by',
    'order by',
    'union all',
    'insert into',
    'delete from',
    'create index',
    'create trigger',
    'create table',
    'create view',
    'case when',
    'after update',
    'after update of',
    'after update on',
);

our @list_tokens = (
    qw/
        as
        in
        is
        not
        null
    /
);

sub _is_multiword_token {
    my ( $self, $all_tokens, $second ) = @_;

    return 0 unless @$all_tokens;
    my $first = $all_tokens->[-1];
    $first  = lc $first;
    $second = lc $second;

    return scalar( grep { "$first $second" eq $_ } @multiword_tokens );
}

sub _is_list_token {
    my ( $self, $token ) = @_;

    my $lc  = lc $token;

    return scalar( grep { $lc eq $_ } @list_tokens );
}

# Standard sort of tokenization.  The only special thing we do here is
# that parentheticals are one giant token.  The tree-maker knows it
# needs to break them up when it comes to them, but this allows our
# tokenization logic to be extra simple.

sub _get_tokens {
    my ( $self, $sql ) = @_;

    # each word is its own token, except if its in a string or comment
    my @tokens            = ();
    my $current_token     = '';
    my $is_in_string      = 0;
    my $is_in_comment     = 0;
    my $is_in_longcomment     = 0;
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
        elsif ($is_in_longcomment) {
            if ( $c eq '/' and $current_token =~ m/\*$/ ) {
                $is_in_longcomment = 0;
                push( @tokens, $current_token .$c);
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
#            elsif (lc($current_token) eq 'begin') {
#                $parenthesis_depth++;
#            }
#            elsif (lc($current_token) eq 'end') {
#                $parenthesis_depth--;
#            }
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
            elsif ( $current_token eq '/*' ) {
                $is_in_longcomment = 1;
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

##################################################

# These functions take lists of tokens and reforge them into tokens
# meant for display

sub list_formatter {
    my ( $self, $major, @minors ) = @_;
    my ( $top, $bottom );

    # Reforge so tokens are separated by commas
    @minors = $self->_reforge_list(@minors);

    # If there's only one column, inline it
    if ( @minors == 1 ) {
        push( @$top, @minors );
    }

    # Special handling for when there's just one subselect in the list,
    # and no other values

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
            push( @current_tokens, $self->uppercase ? uc($token) : $token )
              unless $token eq '__END_OF_SQL';
            push( @new_minors, join( " ", @current_tokens ) );
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
        if ( lc($token) eq 'on' ) {
            push( @new_minors, $self->uppercase ? uc($token) : $token );
            @new_top    = $self->_reforge_list(@new_minors);
            @new_minors = ();
        }
        else {
            push( @new_minors, $token );
        }
    }

    return ( \@new_top, \@new_minors );
}

sub _reforge_list {
    my ( $self, @minors ) = @_;

    # the purpose of the reforge is to comma delimited lines, so we
    # reforge the smaller tokens into the larger tokens used by the
    # display functions.  We must leave things tokenized so we don't
    # reforge on commas inside strings.

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
        elsif ( $self->_is_list_token($token) and $self->uppercase ) {
            push( @current_tokens, uc($token) );
        }
        else {
            push( @current_tokens, $token );
        }
    }

    return @new_minors;
}

sub process_select          { list_formatter(@_) }
sub process_except          { list_formatter(@_) }
sub process_select_distinct { list_formatter(@_) }
sub process_create_table { list_formatter(@_) }
sub process_create_view { list_formatter(@_) }
sub process_case_when { list_formatter(@_) }
sub process_when { list_formatter(@_) }
sub process_then { list_formatter(@_) }
sub process_else { list_formatter(@_) }
sub process_end { list_formatter(@_) }
sub process_end_as { list_formatter(@_) }
sub process_from            { list_formatter(@_) }
sub process_delete_from     { list_formatter(@_) }
sub process_group_by        { list_formatter(@_) }
sub process_order_by        { list_formatter(@_) }
sub process_insert_into     { list_formatter(@_) }
sub process_values          { list_formatter(@_) }
sub process_join            { join_formatter(@_) }
sub process_left_join       { list_formatter(@_) }
sub process_right_join      { join_formatter(@_) }
sub process_inner_join      { list_formatter(@_) }
sub process_outer_join      { join_formatter(@_) }
sub process_full_join       { join_formatter(@_) }
sub process_on       { list_formatter(@_) }
sub process_with       { join_formatter(@_) }
sub process_where           { where_formatter(@_) }
sub process_having          { where_formatter(@_) }
sub process_update          { update_formatter(@_) }
sub process_set             { where_formatter(@_) }
sub process_union           { identity_formatter(@_) }
sub process_union_all       { identity_formatter(@_) }
sub process_limit           { identity_formatter(@_) }
sub process_SLASH           { identity_formatter(@_) }

sub _token_to_sub_name {
    my ( $self, $token ) = @_;

    $token = lc $token;
    if ( $token eq '/' ) {
        $token = 'SLASH';
    }

    my $subroutine = "process_$token";
    $subroutine =~ s/\s/_/g;

    return $subroutine;
}

sub _sql_to_tree {

    # This function breaks things up into chunks that make sense from a
    # token perspective, but not one that knows about keywords.  We
    # have to restructure the chunks a bit to be keyword specific, so
    # that's part of what we do in each major process_$function.

    my ( $self, $sql ) = @_;

    # Find major markers: select, from, where, group, parens
    my @majors         = ();
    my $current_major  = undef;
    my @current_minors = ();

    # For each token, figure out what major it belongs to.  Process
    # majors as we go.
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

                # If there are no majors, its just a list.  If there
                # are no subselects, and there are fewer than 4, then
                # merge them all into a single line.  Otherwise, each
                # element get its own line.  We do it here, instead of
                # in a formatter, because the formatters aren't aware
                # of the concept of "major" and so can't differentiate
                # when to do the inline style.

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

            # if we run into ( ... ), it may have subqueries in it, so
            # parse it as a tree the parenthesis disappear here when
            # the subquery is converted to a tree, but its always added
            # back in when we reforge the list or conditionals

            if ( $token =~ /^\((.*)\)$/s ) {
                my $new_token = $self->_sql_to_tree($1);

                # major blocks will come out as hashes
                my $has_majors   = grep { ref($_) eq 'HASH' } @$new_token;
                my $has_subquery = grep { ref($_) eq 'ARRAY' } @$new_token;

                if ( $has_majors or $has_subquery ) {
                    $token = $new_token;
                }
                else {
                    # Special case for functions that don't have majors
                    # inside of them

                    $token = '(' . join( ' ', @$new_token ) . ')';
                }
            }

            push( @current_minors, $token );
        }

    }

    $self->{branches} = \@majors;
}

sub _get_formatted_branch {

    # Takes a tree, and turns it into nice looking SQL Lots of special
    # cases around, for taste

    my ( $self, $indent_level, $branch ) = @_;

    my $indent     = $self->indent x $indent_level;
    my $sub_indent = $self->indent x ( $indent_level + 1 );

    if ( ref $branch eq 'HASH' ) {

        # this is the major keyword of the branch
        my $name = $self->uppercase ? uc( $branch->{'name'} ) : $branch->{name};

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

                # After the indented array, the next character will be
                # a close-parens.  Don't put a space before it.

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

sub _preserve_leading {
    my ( $output, $original ) = @_;

    # find the index of the left-most non-space character on any line
    my $min_length = 10**10;
    for ( split /\n/, $original ) {
        /^(\s+)/;
        my $cur_length = $1 ? length($1) : 0;
        $min_length = $cur_length < $min_length ? $cur_length : $min_length;

        # shortcut out if we find a zero-indent line
        return $output if $min_length == 0;
    }

    # indent everything by that much
    my $new_output = '';
    my $indent     = " " x $min_length;

    for ( split /\n/, $output ) {
        $new_output .= "$indent$_\n";
    }

    return $new_output;
}

sub tidy {
    my $self = shift;
    my $sql  = shift;

    my $branches = $self->_sql_to_tree($sql);
    print Dumper( $self->{branches} ) if DEBUG;

    my $tidy = join '',
      map { $self->_get_formatted_branch( 0, $_ ); } @$branches;

    _preserve_leading( $tidy, $sql ) if $self->preserve_leading;
    $tidy =~ s!\\!\\\\!gs            if $self->double_escape;
    $tidy;
}

### DO NOT EDIT BELOW! (generated by Class::Inline v0.0.1)
#<<<
  require Carp;our@ATTRS_UNEX=(undef);sub new {my$class=shift;my$self={@_ ? @_
  > 1 ? @_ : %{$_[0]}: ()};if (@ATTRS_UNEX){map {Carp::carp(
  "SQL::Tree argument '$_' unexpected");delete$self->{$_ }}sort grep {not
  exists$INLINE->{$_ }}keys %$self}else {@ATTRS_UNEX=map {delete$self->{$_ };
  $_}grep {not exists$INLINE->{$_ }}keys %$self}bless$self,ref$class || $class
  }sub double_escape {Carp::croak (
  "attribute (SQL::Tree::double_escape) is read-only")if @_ > 1;$_[0]{
  'double_escape'}//= $INLINE->{'double_escape'}->{'default'}}sub indent {
  Carp::croak ("attribute (SQL::Tree::indent) is read-only")if @_ > 1;$_[0]{
  'indent'}//= $INLINE->{'indent'}->{'default'}}sub preserve_leading {
  Carp::croak ("attribute (SQL::Tree::preserve_leading) is read-only")if @_ >
  1;$_[0]{'preserve_leading'}//= $INLINE->{'preserve_leading'}->{'default'}}
  sub uppercase {Carp::croak ("attribute (SQL::Tree::uppercase) is read-only")
  if @_ > 1;$_[0]{'uppercase'}//= $INLINE->{'uppercase'}->{'default'}}
#>>>
### DO NOT EDIT ABOVE! (generated by Class::Inline v0.0.1)

1;

__END__

=head1 NAME

SQL::Tree - parse and prettify SQL text

=head1 SYNOPSIS

    use SQL::Tree;
    my $tree   = SQL::Tree->new(
        indent    => '    ',
        sql       => $sql,
        uppercase => 1,
    );
    my $tidy = $tree->pretty();

=head1 DESCRIPTION

Parses SQL text and reformats it nicely indented.

=head1 CONSTRUCTOR

The C<new()> constructor takes the following arguments:

=over

=item indent (scalar, default is four ' ' spaces)

How to indent each sub-level by when tidying.

=item preserve_leading (boolean, default is true)

Set to keep same overall indent as original.

=item uppercase (boolean, default is true)

Set to uppercase SQL keywords when tidying.

=back

=head1 ATTRIBUTES

=head1 METHODS

=over

=item tidy($sql) -> $tidy_sql

Takes the input SQL text to be parsed and returns a tidy version.

=back

=head1 AUTHOR

Originally by Ryan Kosai <github@ryankosai.com>. Modularized with more
features by Mark Lawrence <nomad@null.net>.

=head1 COPYRIGHT AND LICENSE

Copyright 2011 Ryan Kosai <github@ryankosai.com>

Copyright 2020 Mark Lawrence <nomad@null.net>
