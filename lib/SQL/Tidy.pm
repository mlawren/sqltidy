#!/usr/bin/env perl
package SQL::Tidy;
use strict;
use warnings;
use Carp ();
use Scalar::Util 'refaddr';
use Types::Standard (qw/Maybe Object/);
our $VERSION = '0.0.1';

our $INLINE = {
    CLASS => { inc => 1, },
    curr  => {
        is      => 'rw',
        default => sub { $_[0] }
    },
    latest       => { isa => Maybe [Object], is      => 'rw', weaken => 1, },
    start_indent => { is  => 'rw',           default => '' },
    tokens       => { is  => 'rw',           default => sub { [] }, },
};

my @OPERATORS = ( qw{
    || << >> <= >= == != <> * / % + - & | < > =
} );

my @BOOLOPS = ( qw{
    AND OR
} );

# ORDER is in here extra because of Windows functions
my @STMT = (
    qw/
      PARTITION SAVEPOINT COMPOUND FACTORED ROLLBACK ANALYZE EXPLAIN
      REINDEX RELEASE ATTACH COMMIT CREATE DELETE DETACH INSERT PRAGMA
      SELECT SIMPLE UPDATE VACUUM VALUES ALTER BEGIN ORDER DROP WITH
      /
);

my @INLINE = (
    qw/
      GROUP_CONCAT CONSTRAINT REFERENCES COALESCE DATETIME DEFAULT
      FOREIGN INTEGER NOTNULL PRIMARY VARCHAR COLUMN ISNULL REGEXP UNIQUE
      CHECK INDEX MATCH BLOB CHAR DESC GLOB LIKE NULL OVER TEXT ASC
      INT KEY NOT AS IN IS
      /
);

my @KEYWORDS = (
    qw/
      CURRENT_TIMESTAMP AUTOINCREMENT CURRENT_DATE CURRENT_TIME
      TRANSACTION DEFERRABLE EXCLUSIVE FOLLOWING GENERATED IMMEDIATE
      INITIALLY INTERSECT PRECEDING RECURSIVE TEMPORARY UNBOUNDED
      CONFLICT DATABASE DEFERRED DISTINCT RESTRICT BETWEEN CASCADE
      COLLATE CURRENT EXCLUDE INDEXED INSTEAD NATURAL NOTHING REPLACE
      TRIGGER VIRTUAL WITHOUT ACTION ALWAYS BEFORE ESCAPE EXCEPT EXISTS
      FILTER GROUPS HAVING IGNORE OFFSET OTHERS RENAME VALUES WINDOW
      ABORT AFTER CROSS FIRST GROUP INNER LIMIT NULLS OUTER QUERY RAISE
      RANGE RIGHT TABLE UNION USING WHERE CASE CAST EACH ELSE FAIL FROM
      FULL INTO JOIN LAST LEFT PLAN ROWS TEMP THEN TIES VIEW WHEN ADD
      ALL END FOR ROW SET BY DO IF NO OF ON TO
      /
);

my $stmt_re      = join '|', @STMT;
my $keywords_re  = join '|', @KEYWORDS;
my $inline_re    = join '|', @INLINE;
my $boolop_re    = join '|', @BOOLOPS;
my $operators_re = join '|', map { s!([|/*+])!\\$1!gr } @OPERATORS;

my $scomment_re = qr/ \n? \ * -- [^\n]* /sx;
my $comment_re  = qr/ \/\* .*? (?: \*\/ | $ ) /sx;
my $shell_re    = qr/ ^\. [^\n]* \n? /mx;
my $passthru_re = qr/ $comment_re | $shell_re /x;
my $num_re      = qr/  (?: [0-9]+ (?: \. [0-9]*)? (?: e[+-]? [0-9]+ )? )
                     | (?: \. [0-9]+ (?: e[+-]? [0-9]+ )? ) /x;
my $word_re       = qr/ [a-zA-Z] [_a-zA-Z0-9]* /x;
my $identifier_re = qr/ (?: " [^"]* " | $word_re ) /x;
my $column_re     = qr/ (?: $identifier_re \.){0,2} (?: $identifier_re | \* )/x;
my $string_re     = qr/ [xX]? ' (?: '' | [^'] )* ' /x;
my $expr_re = qr/ $column_re | $string_re | $operators_re | $num_re | \?  /x;

our $__doc;
my $re = qr!
  (?:
      \b($stmt_re)\b     (?{ $__doc->start_stmt($^N); })
    | (\))               (?{ $__doc->end_block( $^N ); })
    | \b(CASE)\b         (?{ $__doc->start_case(); })
    | \b(END)\b          (?{ $__doc->end_case(); })
    | \b($keywords_re)\b (?{ $__doc->add_keyword($^N); })
    | \b($inline_re)\b   (?{ $__doc->add_inline($^N); })
    | \b($boolop_re)\b   (?{ $__doc->add_boolop($^N); })
    | ($word_re\s*\()    (?{ $__doc->start_function( $^N, ')' ); })
    | \(                 (?{ $__doc->start_block( '(', ')' ); })
    | ($scomment_re)     (?{ $__doc->add_comment($^N); })
    | ($passthru_re)\n?  (?{ $__doc->add_passthru($^N); })
    | ($expr_re)         (?{ $__doc->add_expr($^N); })
    | ,                  (?{ $__doc->make_list; })
    | \n                 (?{ $__doc->add_newline; })
    | (\s+)              (?{ $__doc->add_ws($^N); })
    | ;                  (?{ $__doc->end_stmt(';'); })
    | (.)                (?{ $__doc->add_rest($^N); })
  )
!ix;

sub curr_isa {
    my $self = shift;
    my $type = shift // '*undef*';
    $type eq ref( $self->curr );
}

sub latest_isa {
    my $self = shift;
    my $type = shift // '*undef*';
    $type eq ref( $self->latest );
}

sub _reset {
    my $self = shift;
    $self->curr($self);
    $self->latest(undef);
    $self->start_indent('');
    $self->tokens( [] );
}

sub tidy {
    my $self = shift;
    my $sql  = shift;
    $self->parse($sql);
    $self->tree2sql;
}

sub parse {
    my $self  = shift;
    my $messy = shift;
    $self->_reset;
    local $__doc = $self;

    die "coult not parse input as SQLite SQL\n"
      unless my @list = $messy =~ m/$re/g;
}

sub tree2ast {
    my $self  = shift;
    my $clean = '';
    my $depth = '';
    my @tok   = map { [ $depth, $_ ] } @{ $self->tokens };
    while ( my $t = shift @tok ) {
        if ( ref( $t->[1] ) and $t->[1]->can('tokens') ) {
            if ( $t->[1]->can('val') ) {
                $clean .= '--| '
                  . $t->[0]
                  . ref( $t->[1] ) . ': '
                  . $t->[1]->val . "\n";
            }
            else {
                $clean .= '--| ' . $t->[0] . ref( $t->[1] ) . ': ' . "\n";
            }
            unshift @tok, map { [ $t->[0] . '  ', $_ ] } @{ $t->[1]->tokens };
        }
        else {
            #                warn $t->[0] . $t->[1] . "\n";
        }
    }
    $clean;
}

sub tree2sql {
    my $self  = shift;
    my $clean = '';
    my $extra = '    ';
    my $NL    = ["\n"];
    my $WS    = [' '];
    my @tok   = map { [$_] } @{ $self->tokens };

    my $prev = '';
    while ( my $t = shift @tok ) {
        my ( $n, $ref, $indent ) = ( $t->[0], ref( $t->[0] ), $t->[1] // '' );
        my $newindent = $indent . $extra;
        my @new;

        if ( $ref eq 'Statement' ) {
            my $c_indent = ( $clean !~ m/\n$/ && $clean =~ m/( +)$/ ) ? $1 : '';
            $indent    = length($c_indent) ? $c_indent : $indent;
            $newindent = $indent . $extra;                         # recalculate
            my @list = @{ $n->tokens };
            my $i    = 0;
            while ( my $t = shift @list ) {
                $ref = ref($t);
                $i++;
                my $nref = ref $list[0] // '';

                if ( ref($t) eq 'Keywords' ) {
                    if ( length $c_indent ) {
                        push @new, [$t];
                        $c_indent = '';
                    }

                    #                    elsif ( $i == 1 ) {
                    #                        push @new, [$indent], [$t];
                    #                    }
                    else {
                        push @new, $NL, [$indent], [$t];
                    }
                }
                elsif ( $ref eq 'List' ) {
                    push @new, [ $t, $newindent ];
                }
                elsif ( $ref eq 'Expr' ) {
                    push @new, $WS, [ $t, $newindent ];
                }
                elsif ( $ref eq 'Function' ) {
                    push @new, $WS, [ $t, $indent ];
                }
                elsif ( $ref eq 'Block' ) {
                    push @new, $WS, [ $t, $indent . $extra ];
                }
                elsif ( $ref eq 'Tokens' ) {
                    push @new, [ $t, $newindent ];
                }
                elsif ( $ref eq 'NotSQL' ) {    # generally a comment?
                    push @new, [$indent], [ $t, $newindent ];
                }
                elsif ( $ref eq 'Comment' ) {
                    push @new, $WS, [$t];
                }
                else {
                    warn 'unhandled ' . $ref;
                }
                $prev = $ref;
            }
        }
        elsif ( $ref eq 'List' ) {
            my @items = @{ $n->tokens };

   #            if (not grep {'Tokens' ne ref $_} map {@{$_->tokens} } @items) {
   #                my $first  = shift @items;
   #                push @new, [ $first, $newindent ];
   #                foreach my $t (@items) {
   #                    push @new, [','], $WS, [ $t, $newindent ];
   #                }
   #            }
   #            else {
            my $first = shift @items;
            push @new, $NL, [$indent], [ $first, $newindent ];
            my $COMMA = [','];
            foreach my $t (@items) {
                if ( ref($t) eq 'Comment' ) {
                    push @new, $COMMA, [$t];
                    $COMMA = [''];
                }
                else {
                    push @new, $COMMA, $NL, [$indent], [ $t, $newindent ];
                    $COMMA = [','];
                }
            }

            #            }
        }
        elsif ( $ref eq 'Block' or $ref eq 'Function' ) {
            my @list = @{ $n->tokens };
            my $ref  = ref $list[0];
            if ( not @list ) {
                push @new, [ $n->val . $n->postval ];
            }
            elsif ( 'List' eq ( my $ref = ref $list[0] ) ) {
                push @new, [ $n->val ],
                  ( map { [ $_, $indent ] } @list ),
                  $NL, [ '  ' . $indent =~ s/$extra//r ], [ $n->postval ];
            }
            elsif ( $ref eq 'Statement' ) {
                push @new, [ $n->val ],
                  ( map { [ $_, $indent ] } @list ),
                  $NL, [ '  ' . $indent =~ s/$extra//r ], [ $n->postval ];
            }
            else {    # Expr? Keywords (for things like Window Functions)
                push @new, [ $n->val ], [ ( shift @list ), $indent ];
                foreach my $t (@list) {
                    push @new, $WS, [ $t, $indent ];
                }
                push @new, [ $n->postval ];
            }
        }
        elsif ( $ref eq 'Expr' ) {
            my @list = @{ $n->tokens };
            my $i    = 1;
            foreach my $t (@list) {
                if ( ref($t) eq 'Block' ) {
                    push @new, ( $i > 1 ? $WS : () ), [ $t, $indent ];
                }
                elsif ( ref($t) eq 'Statement' ) {    # CASE ... END
                    push @new, [ $t, $indent ];
                }
                elsif ( $i == 1 ) {
                    push @new, [ $t, $indent ];
                }
                elsif ( ref($t) eq 'BoolOp' ) {
                    push @new, $NL, [$indent], [ $t, $indent ];
                }
                else {
                    push @new, $WS, [ $t, $indent ];
                }
                $i++;
            }
        }
        elsif ( $ref eq 'Keywords' or $ref eq 'Inline' or $ref eq 'BoolOp' ) {
            $clean .= uc "@{$n->tokens}";
        }
        elsif ( $ref eq 'Tokens' ) {
            $clean .= "@{$n->tokens}";
        }
        elsif ( $ref eq 'Comment' ) {
            $clean .= $n->val;
        }
        elsif ( $ref eq 'NotSQL' ) {
            $clean .= join '', @{ $n->tokens };
        }
        else {
            #            warn "unhandled X${n}X" unless $n eq ' ';
            $clean .= $indent . $n;
        }
        unshift @tok, @new;
    }
    return $clean;
}

sub add {
    my $self = shift;
    my $val  = shift;
    Carp::confess 'junk not a ref ' . $val unless ref $val;

    if ( refaddr( $self->curr ) != refaddr($self) ) {
        $self->curr->add($val);
    }
    else {
        push @{ $self->tokens }, $val;
    }

    $self->latest($val);
}

sub start_function {
    my $self  = shift;
    my $start = shift;
    my $end   = shift;

    unless ( $self->curr_isa('Expr') ) {
        my $e = Expr->new( parent => $self->curr );
        $self->add($e);
        $self->curr($e);
    }

    my $block = Function->new(
        val     => $start,
        postval => $end,
        parent  => $self->curr,
    );

    $self->add($block);
    $self->curr($block);
}

sub start_case {
    my $self = shift;
    unless ( $self->curr_isa('Expr') ) {
        my $e = Expr->new( parent => $self->curr );
        $self->add($e);
        $self->curr($e);
    }

    $self->start_block(
        Keywords->new(
            val    => 'case',
            parent => $self->curr,
        ),
        'end'
    );
    my $stmt = Statement->new( parent => $self->curr );
    $self->add($stmt);
    $self->curr($stmt);
}

sub end_case {
    my $self = shift;

    #    $self->add_keyword('end');
    my $curr = $self->curr;
    while ( refaddr($curr) != refaddr($self) ) {
        if ( $curr->isa('Block') && lc( $curr->postval ) eq lc('end') ) {
            my $end = Keywords->new( val => 'end', parent => $curr );
            $curr->postval($end);
            $self->curr( $curr->parent // $self );
            $self->latest( $self->curr );
            return;
        }
        $curr = $curr->parent // last;
    }

    # did not find the start block! Warn?
}

sub start_block {
    my $self  = shift;
    my $start = shift;
    my $end   = shift;

    unless ( $self->curr_isa('Expr') ) {
        my $e = Expr->new( parent => $self->curr );
        $self->add($e);
        $self->curr($e);
    }

    my $block = Block->new(
        val     => $start,
        postval => $end,
        parent  => $self->curr,
    );

    $self->add($block);
    $self->curr($block);
}

sub end_block {
    my $self = shift;
    my $end  = shift;
    if ( my $curr = $self->curr ) {
        while ( refaddr($curr) != refaddr($self) ) {
            if ( $curr->isa('Block') && lc( $curr->postval ) eq lc($end) ) {
                $self->curr( $curr->parent // $self );
                $self->latest( $self->curr );
                return;
            }
            $curr = $curr->parent // last;
        }
    }

    # did not find the start block!
    $self->add(
        Tokens->new( val => $end . ' -- UNEXPECTED', parent => $self->curr ) );
}

sub add_comment {
    my $self = shift;
    my $val  = shift;
    $self->add( Comment->new(
        val    => $val,
        parent => $self->curr,
    ) );
}

sub add_passthru {
    my $self = shift;
    my $val  = shift;
    $self->add( NotSQL->new(
        val    => $val,
        parent => $self->curr,
    ) );
}

sub add_token {
    my $self = shift;
    my $val  = shift;
    $self->add( Tokens->new(
        val    => $val,
        parent => $self->curr,
    ) );
}

sub in_statement {
    my $self = shift;
    if ( my $curr = $self->curr ) {
        while ( refaddr($curr) != refaddr($self) ) {
            return $curr if $curr->isa('Statement');
            $curr = $curr->parent // last;
        }
    }
    return 0;
}

sub add_keyword {
    my $self = shift;
    my $val  = shift;

    if (    $self->latest_isa('Keywords')
        and $self->latest->parent eq $self->curr )
    {
        $self->latest->add($val);
        return;
    }
    else {    # break out of a List or Expr
        until (
                 $self->curr_isa('Statement')
              or $self->curr_isa('Block')
              or refaddr( $self->curr ) == refaddr($self)
          )
        {
            $self->curr( $self->curr->parent );
        }
    }

    $self->add( Keywords->new(
        val    => $val,
        parent => $self->curr,
    ) );
}

sub start_stmt {
    my $self = shift;
    my $val  = shift;

    if ( $self->curr_isa('Expr') ) {    #special case for INSERT ... SELECT
        $self->curr( $self->curr->parent );
    }
    elsif ( not $self->curr_isa('Statement') ) {
        my $stmt = Statement->new( parent => $self->curr );
        $self->add($stmt);
        $self->curr($stmt);
    }
    $self->add_keyword($val);
}

sub end_stmt {
    my $self = shift;
    my $val  = shift;

    if ( my $stmt = $self->in_statement ) {
        $stmt->add( Tokens->new(
            val    => $val,
            parent => $self->curr,
        ) );
        $self->curr( $stmt->parent );
        return;
    }

    # warn or not?
    $self->add( Tokens->new(
        val    => $val,
        parent => $self->curr,
    ) );
}

sub add_newline {
    my $self = shift;
    return if $self->in_statement;

    $self->add( Tokens->new(
        val    => "\n",
        parent => $self->curr,
    ) );
}

sub add_ws {
    my $self = shift;
    my $val  = shift;
    return if $self->in_statement;
    $self->add( Tokens->new( val => $val, parent => $self->curr ) );
    $self->start_indent($val);
}

sub add_boolop {
    my $self = shift;
    my $val  = shift;
    $self->add( BoolOp->new( val => $val, parent => $self->curr, ) );
}

sub add_inline {
    my $self = shift;
    my $val  = shift;

    if ( $self->latest_isa('Inline') ) {
        $self->latest->add($val);
        return;
    }

    unless ( $self->curr_isa('Expr') ) {

        #    warn $val .' '.$self->curr;
        unless ( $self->in_statement ) {
            my $stmt = Statement->new( parent => $self->curr, );
            $self->add($stmt);
            $self->curr($stmt);
        }

        my $e = Expr->new( parent => $self->curr );
        $self->add($e);
        $self->curr($e);
    }

    $self->add( Inline->new( val => $val, parent => $self->curr, ) );
}

sub make_list {
    my $self   = shift;
    my $parent = $self->curr->parent;

    if ( ref($parent) eq 'List' ) {
        $self->curr($parent);
        $self->latest($parent);
    }
    elsif ( $self->curr_isa('Expr') ) {
        my $bottom = delete $parent->tokens->[-1];
        my $middle = List->new( parent => $parent );
        $parent->add($middle);
        $middle->add($bottom);
        $self->curr($middle);
        $self->latest($middle);
    }
    elsif ( $self->curr_isa('Statement') ) {
        my $bottom = delete $self->curr->tokens->[-1];
        my $list   = List->new( parent => $self->curr );
        $self->add($list);
        $list->add($bottom);
        $self->curr($list);
        $self->latest($bottom);
    }
    else {
        warn 'unhandled make_list with ' . $parent . $self->curr;
    }
}

sub add_expr {
    my $self = shift;
    my $val  = shift;

    unless ( $self->curr_isa('Expr') ) {
        my $e = Expr->new( parent => $self->curr );
        $self->add($e);
        $self->curr($e);
    }
    $self->add( Tokens->new( val => $val, parent => $self->curr, ) );
}

sub add_rest {
    my $self = shift;
    my $val  = shift;
    $self->add( Tokens->new(
        val    => $val,
        parent => $self->curr,
    ) );
}

### DO NOT EDIT BELOW! (generated by Class::Inline v0.0.1)
#<<<
  require Carp;require Scalar::Util;our@ATTRS_UNEX=(undef);sub new {my$class=
  shift;my$self={@_ ? @_ > 1 ? @_ : %{$_[0]}: ()};if (@ATTRS_UNEX){map {local
  $Carp::CarpLevel=$Carp::CarpLevel + 1;Carp::carp(
  "SQLDoc attribute '$_' unexpected");delete$self->{$_ }}sort grep {not exists
  $INLINE->{$_ }}keys %$self}else {@ATTRS_UNEX=map {delete$self->{$_ };$_}grep
  {not exists$INLINE->{$_ }}keys %$self}bless$self,ref$class || $class;map {
  $self->{$_ }=eval {$INLINE->{$_ }->{'isa'}->($self->{$_ })};Carp::croak(
  qq{SQLDoc::$_ value invalid ($@)})if $@}grep {exists$self->{$_ }}'latest';
  map {Scalar::Util::weaken($self->{$_ })}grep {defined$self->{$_ }// undef}
  'latest';$self}sub __ro {my (undef,undef,undef,$sub)=caller(1);local
  $Carp::CarpLevel=$Carp::CarpLevel + 1;Carp::croak(
  "attribute $sub is read-only (value: '" .($_[1]// 'undef')."')")}sub curr {
  if (@_ > 1){$_[0]{'curr'}=$_[1];return $_[0]}$_[0]{'curr'}//= $INLINE->{
  'curr'}->{'default'}->($_[0])}sub debug {$_[0]->__ro($_[1])if @_ > 1;$_[0]{
  'debug'}// undef}sub latest {if (@_ > 1){$_[0]{'latest'}=eval {$INLINE->{
  'latest'}->{'isa'}->($_[1])};Carp::croak('invalid (SQLDoc::latest) value: '.
  $@)if $@;Scalar::Util::weaken($_[0]{'latest'})if defined $_[1];return $_[0]}
  $_[0]{'latest'}// undef}sub start_indent {if (@_ > 1){$_[0]{'start_indent'}=
  $_[1];return $_[0]}$_[0]{'start_indent'}//= $INLINE->{'start_indent'}->{
  'default'}}sub tokens {if (@_ > 1){$_[0]{'tokens'}=$_[1];return $_[0]}$_[0]{
  'tokens'}//= $INLINE->{'tokens'}->{'default'}->($_[0])}BEGIN{$INC{
  'SQLDoc.pm'}//= __FILE__}
#>>>
### DO NOT EDIT ABOVE! (generated by Class::Inline v0.0.1)

package Tokens;
use strict;
use warnings;
use Types::Standard (qw/Defined/);

our $INLINE = {
    CLASS  => { inc      => 1, },
    parent => { required => 1, weaken => 1, },
    val    => { isa      => Defined, required => 1, },
    tokens => { default  => sub { [ $_[0]->val ] }, },
};

sub add {
    my $self = shift;
    my $val  = shift;
    push( @{ $self->tokens }, $val );

}

BEGIN { $INC{'Tokens.pm'} = __FILE__; }

### DO NOT EDIT BELOW! (generated by Class::Inline v0.0.1)
#<<<
  require Carp;require Scalar::Util;our@ATTRS_UNEX=(undef);sub new {my$class=
  shift;my$self={@_ ? @_ > 1 ? @_ : %{$_[0]}: ()};map {local$Carp::CarpLevel=
  $Carp::CarpLevel + 1;Carp::croak("missing attribute Tokens::$_ is required")
  unless exists$self->{$_}}'parent','val';if (@ATTRS_UNEX){map {local
  $Carp::CarpLevel=$Carp::CarpLevel + 1;Carp::carp(
  "Tokens attribute '$_' unexpected");delete$self->{$_ }}sort grep {not exists
  $INLINE->{$_ }}keys %$self}else {@ATTRS_UNEX=map {delete$self->{$_ };$_}grep
  {not exists$INLINE->{$_ }}keys %$self}bless$self,ref$class || $class;map {
  $self->{$_ }=eval {$INLINE->{$_ }->{'isa'}->($self->{$_ })};Carp::croak(
  qq{Tokens::$_ value invalid ($@)})if $@}grep {exists$self->{$_ }}'val';map {
  Scalar::Util::weaken($self->{$_ })}grep {defined$self->{$_ }// undef}
  'parent';$self}sub __ro {my (undef,undef,undef,$sub)=caller(1);local
  $Carp::CarpLevel=$Carp::CarpLevel + 1;Carp::croak(
  "attribute $sub is read-only (value: '" .($_[1]// 'undef')."')")}sub parent
  {$_[0]->__ro($_[1])if @_ > 1;$_[0]{'parent'}}sub tokens {$_[0]->__ro($_[1])
  if @_ > 1;$_[0]{'tokens'}//= $INLINE->{'tokens'}->{'default'}->($_[0])}sub
  val {$_[0]->__ro($_[1])if @_ > 1;$_[0]{'val'}}BEGIN{$INC{'Tokens.pm'}//=
  __FILE__}
#>>>
### DO NOT EDIT ABOVE! (generated by Class::Inline v0.0.1)

package NotSQL;
use parent 'Tokens';

package Comment;
use parent 'Tokens';

package Keywords;
use strict;
use warnings;
use parent 'Tokens';

package Inline;
use strict;
use warnings;
use parent 'Tokens';

package BoolOp;
use strict;
use warnings;
use parent 'Tokens';

package Expr;
use strict;
use warnings;

our $INLINE = {
    CLASS  => { inc      => 1, },
    parent => { required => 1, weaken => 1, },
    tokens => { is       => 'rw', default => sub { [] }, },
};

sub add {
    my $self = shift;
    my $val  = shift;
    push @{ $self->tokens }, $val;
}

### DO NOT EDIT BELOW! (generated by Class::Inline v0.0.1)
#<<<
  require Carp;require Scalar::Util;our@ATTRS_UNEX=(undef);sub new {my$class=
  shift;my$self={@_ ? @_ > 1 ? @_ : %{$_[0]}: ()};map {local$Carp::CarpLevel=
  $Carp::CarpLevel + 1;Carp::croak("missing attribute Expr::$_ is required")
  unless exists$self->{$_}}'parent';if (@ATTRS_UNEX){map {local
  $Carp::CarpLevel=$Carp::CarpLevel + 1;Carp::carp(
  "Expr attribute '$_' unexpected");delete$self->{$_ }}sort grep {not exists
  $INLINE->{$_ }}keys %$self}else {@ATTRS_UNEX=map {delete$self->{$_ };$_}grep
  {not exists$INLINE->{$_ }}keys %$self}bless$self,ref$class || $class;map {
  Scalar::Util::weaken($self->{$_ })}grep {defined$self->{$_ }// undef}
  'parent';$self}sub __ro {my (undef,undef,undef,$sub)=caller(1);local
  $Carp::CarpLevel=$Carp::CarpLevel + 1;Carp::croak(
  "attribute $sub is read-only (value: '" .($_[1]// 'undef')."')")}sub parent
  {$_[0]->__ro($_[1])if @_ > 1;$_[0]{'parent'}}sub tokens {if (@_ > 1){$_[0]{
  'tokens'}=$_[1];return $_[0]}$_[0]{'tokens'}//= $INLINE->{'tokens'}->{
  'default'}->($_[0])}BEGIN{$INC{'Expr.pm'}//= __FILE__}
#>>>
### DO NOT EDIT ABOVE! (generated by Class::Inline v0.0.1)

package List;
use strict;
use warnings;

use Class::Inline {
    CLASS  => { inc      => 1, },
    parent => { required => 1, weaken => 1, },
    tokens => { is       => 'rw', default => sub { [] }, },
};

sub add {
    my $self = shift;
    my $val  = shift;
    return push @{ $self->tokens }, $val;
}

package Statement;
use strict;
use warnings;

our $INLINE = {
    CLASS  => { inc      => 1, },
    parent => { required => 1, },
    tokens => { is       => 'rw', default => sub { [] }, },

    #    lead   => { required => 1, },
};

sub add {
    my $self = shift;
    my $val  = shift;
    push @{ $self->tokens }, $val;
}

### DO NOT EDIT BELOW! (generated by Class::Inline v0.0.1)
#<<<
  require Carp;our@ATTRS_UNEX=(undef);sub new {my$class=shift;my$self={@_ ? @_
  > 1 ? @_ : %{$_[0]}: ()};map {local$Carp::CarpLevel=$Carp::CarpLevel + 1;
  Carp::croak("missing attribute Statement::$_ is required")unless exists$self
  ->{$_}}'parent';if (@ATTRS_UNEX){map {local$Carp::CarpLevel=$Carp::CarpLevel
  + 1;Carp::carp("Statement attribute '$_' unexpected");delete$self->{$_ }}
  sort grep {not exists$INLINE->{$_ }}keys %$self}else {@ATTRS_UNEX=map {
  delete$self->{$_ };$_}grep {not exists$INLINE->{$_ }}keys %$self}bless$self,
  ref$class || $class}sub __ro {my (undef,undef,undef,$sub)=caller(1);local
  $Carp::CarpLevel=$Carp::CarpLevel + 1;Carp::croak(
  "attribute $sub is read-only (value: '" .($_[1]// 'undef')."')")}sub parent
  {$_[0]->__ro($_[1])if @_ > 1;$_[0]{'parent'}}sub tokens {if (@_ > 1){$_[0]{
  'tokens'}=$_[1];return $_[0]}$_[0]{'tokens'}//= $INLINE->{'tokens'}->{
  'default'}->($_[0])}BEGIN{$INC{'Statement.pm'}//= __FILE__}
#>>>
### DO NOT EDIT ABOVE! (generated by Class::Inline v0.0.1)

package Block;
use strict;
use warnings;

our $INLINE = {
    CLASS   => { inc      => 1, },
    val     => { required => 1, },
    parent  => { required => 1, weaken => 1, },
    postval => { is       => 'rw', required => 1, },
    tokens  => { is       => 'rw', default => sub { [] }, },
};

sub add {
    my $self = shift;
    my $val  = shift;
    push @{ $self->tokens }, $val;
}

### DO NOT EDIT BELOW! (generated by Class::Inline v0.0.1)
#<<<
  require Carp;require Scalar::Util;our@ATTRS_UNEX=(undef);sub new {my$class=
  shift;my$self={@_ ? @_ > 1 ? @_ : %{$_[0]}: ()};map {local$Carp::CarpLevel=
  $Carp::CarpLevel + 1;Carp::croak("missing attribute Block::$_ is required")
  unless exists$self->{$_}}'parent','postval','val';if (@ATTRS_UNEX){map {
  local$Carp::CarpLevel=$Carp::CarpLevel + 1;Carp::carp(
  "Block attribute '$_' unexpected");delete$self->{$_ }}sort grep {not exists
  $INLINE->{$_ }}keys %$self}else {@ATTRS_UNEX=map {delete$self->{$_ };$_}grep
  {not exists$INLINE->{$_ }}keys %$self}bless$self,ref$class || $class;map {
  Scalar::Util::weaken($self->{$_ })}grep {defined$self->{$_ }// undef}
  'parent';$self}sub __ro {my (undef,undef,undef,$sub)=caller(1);local
  $Carp::CarpLevel=$Carp::CarpLevel + 1;Carp::croak(
  "attribute $sub is read-only (value: '" .($_[1]// 'undef')."')")}sub parent
  {$_[0]->__ro($_[1])if @_ > 1;$_[0]{'parent'}}sub postval {if (@_ > 1){$_[0]{
  'postval'}=$_[1];return $_[0]}$_[0]{'postval'}}sub tokens {if (@_ > 1){$_[0]
  {'tokens'}=$_[1];return $_[0]}$_[0]{'tokens'}//= $INLINE->{'tokens'}->{
  'default'}->($_[0])}sub val {$_[0]->__ro($_[1])if @_ > 1;$_[0]{'val'}}BEGIN{
  $INC{'Block.pm'}//= __FILE__}
#>>>
### DO NOT EDIT ABOVE! (generated by Class::Inline v0.0.1)

package Function;
use parent 'Block';

1;

__END__

=head1 NAME

SQL::Tidy - parse and prettify SQL text

=head1 SYNOPSIS

    use SQL::Tidy;
    my $sqlt   = SQL::Tidy->new(
        indent    => 4,
    );
    my $sql = $sqlt->parse($SQL);

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

=item parse($sql) -> $tidy_sql

Takes the input SQL text to be parsed and returns a tidy version.

=back

=head1 AUTHOR

Mark Lawrence <nomad@null.net>.

=head1 COPYRIGHT AND LICENSE

Copyright 2020 Mark Lawrence <nomad@null.net>

