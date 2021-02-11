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
    indent => { is  => 'rw',           default => 4, },
    latest => { isa => Maybe [Object], is      => 'rw', weaken => 1, },
    tokens => { is  => 'rw',           default => sub { [] }, },
};

sub sqltidy {
    my $messy = shift // Carp::croak 'sqltidy: input undefined';
    my $sql   = __PACKAGE__->new(@_);
    $sql->parse($messy);
    $sql->tree2sql;
}

my $ws_re       = qr/ [\ \t] /x;
my $leadws_re   = qr/ (?m) ^ $ws_re+ /sx;
my $wb_re       = qr/ \b | [\ \n \t] | $ /sx;
my $scomment_re = qr/ $ws_re* -- \N* \n? /sx;
my $comment_re  = qr/ \n? \/\* .*? (?: \*\/ | $ ) \n? /sx;
my $shell_re    = qr/ (?m) ^\. \N* \n? /mx;
my $passthru_re = qr/ $comment_re | $shell_re /x;
my $num_re      = qr/  (?: [0-9]+ (?: \. [0-9]*)? (?: e[+-]? [0-9]+ )? )
                     | (?: \. [0-9]+ (?: e[+-]? [0-9]+ )? ) /x;
my $word_re       = qr/ [_a-zA-Z] [_a-zA-Z0-9]* /x;
my $identifier_re = qr/ (?: " [^"]* " | :? $word_re ) /x;
my $column_re     = qr/ (?: $identifier_re \.){0,2} (?: $identifier_re | \* )/x;
my $string_re     = qr/ [xX]? ' (?: '' | [^'] )* ' /x;

my @OPERATORS = ( qw{
    || << >> <= >= == != <> * / % + - & | < > =
} );

my @BOOLOPS = ( qw{
    AND OR
} );

# ORDER RANGE PARTITION EXLUDE ROWS GROUPS here because of Windows functions
my @STMT = (
    qw/
      PARTITION SAVEPOINT COMPOUND FACTORED ROLLBACK ANALYZE EXPLAIN
      REINDEX RELEASE ATTACH COMMIT CREATE DELETE DETACH INSERT PRAGMA
      SELECT SIMPLE UPDATE VACUUM VALUES ALTER BEGIN ORDER DROP WITH
      RANGE ROWS GROUPS EXCLUDE WHEN
      /
);

# should be seen as function not keyword:
# GROUP_CONCAT COALESCE REPLACE. The problem is with SELECT COALESCE(1,2)

my @INLINE = (
    qw/
      CONSTRAINT REFERENCES DATETIME DEFAULT
      FOREIGN INTEGER NOTNULL PRIMARY VARCHAR COLUMN ISNULL REGEXP UNIQUE
      CHECK INDEX MATCH BLOB CHAR DESC GLOB LIKE NULL OVER TEXT ASC
      INT KEY NOT AS IN IS

      COLLATE
      CASCADE UNBOUNDED

      CURRENT_TIMESTAMP AUTOINCREMENT CURRENT_DATE CURRENT_TIME
      TRANSACTION DEFERRABLE EXCLUSIVE FOLLOWING GENERATED IMMEDIATE
      INITIALLY INTERSECT PRECEDING RECURSIVE TEMPORARY
      CONFLICT DATABASE DEFERRED DISTINCT RESTRICT BETWEEN
      CURRENT INDEXED NATURAL NOTHING
      TRIGGER VIRTUAL WITHOUT ACTION ALWAYS BEFORE ESCAPE EXCEPT EXISTS
      FILTER IGNORE OTHERS STORED
      ABORT FIRST NULLS OUTER QUERY RAISE
      TABLE USING CAST EACH FAIL
      FULL INTO LAST PLAN TEMP TIES VIEW
      ALL END ROW BY IF NO OF TO

      /, 'ON DELETE', 'ON UPDATE', 'DEFAULT VALUES', 'OR IGNORE', 'OR REPLACE'
);

my @KEYWORDS = (
    qw/
      FROM INNER LEFT RIGHT JOIN WHERE GROUP HAVING UNION
      THEN ELSE CASE END ON SET FOR INSTEAD LIMIT OFFSET ADD DO RENAME
      CROSS WINDOW RETURNING AFTER
      /
);

sub _lenorder {
    map    { s/ +/$ws_re+/gr }           # more than single space for whitespace
      map  { s{([|/*+])}{\\$1}gr }       # escape regex chars
      sort { length $b <=> length $a }   # longest to shortest
      @_;
}

my $stmt_re     = join '|', _lenorder @STMT;
my $keywords_re = join '|', _lenorder @KEYWORDS;
my $inline_re   = join '|', _lenorder @INLINE;
my $boolop_re   = join '|', _lenorder @BOOLOPS;
my $op_re       = join '|', _lenorder @OPERATORS;

my $expr_re = qr/ $column_re | $string_re | $op_re | $num_re | \?  /x;

our $__doc;

my $re = qr!
  (?:
      (BEGIN)$wb_re        (?{ $__doc->start_begin($^N);            })
    | ($stmt_re)$wb_re     (?{ $__doc->start_stmt($^N);             })
    | \(                   (?{ $__doc->start_block( qw/ ( ) / );    })
    | (\))                 (?{ $__doc->end_block( $^N );            })
    | (CASE)$wb_re         (?{ $__doc->start_case();                })
    | (END)$wb_re          (?{ $__doc->end_block( $^N );            })
    | ($inline_re)$wb_re   (?{ $__doc->add_inline($^N);             })
    | ($keywords_re)$wb_re (?{ $__doc->add_keyword($^N);            })

    # Needs to be before $op_re so that '-' does get matched early
    | ($scomment_re)       (?{ $__doc->add_comment($^N);            })
    | ($op_re)             (?{ $__doc->add_op($^N);                 })
    | ($boolop_re)$wb_re   (?{ $__doc->add_boolop($^N);             })
    | ($word_re)\s*\(      (?{ $__doc->start_function( $^N.'(', ')' ); })
    | ($passthru_re)       (?{ $__doc->add_passthru($^N);           })
    | ($expr_re)           (?{ $__doc->add_expr($^N);               })
    | ,                    (?{ $__doc->make_list;                   })
    | ;                    (?{ $__doc->end_stmt(';');               })
    | ($leadws_re)         (?{ $__doc->add_leadws($^N);             })
    | (\s*\n | $ws_re+)    (?{ $__doc->add_ws($^N);                 })
    | (.)                  (?{ $__doc->add_rest($^N);               })
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
                  . ref( $t->[1] ) . ': \''
                  . $t->[1]->val . "'\n";
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
    my $extra = ' ' x $self->indent;
    my $half  = ' ' x ( $self->indent / 2 );
    my $NL    = ["\n"];
    my $WS    = [' '];
    my @tok   = map { [$_] } @{ $self->tokens };

    my $lead_ws = '';
    my $tok     = 0;
    while ( my $t = shift @tok ) {
        my ( $n, $ref, $indent ) =
          ( $t->[0], ref( $t->[0] ), $t->[1] // $lead_ws );
        my $newindent = $indent . $extra;
        my @new;
        $lead_ws = '';

        if ( $ref eq 'Statement' ) {
            my @list = @{ $n->tokens };
            my $i    = 0;
            while ( my $t = shift @list ) {
                $ref = ref($t);
                $i++;

                if ( ref($t) eq 'Keywords' ) {
                    push @new, $i > 1 ? $NL : (), [$indent], [$t];
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
                    push @new, ( length $t->val ? $WS : () ),
                      [ $t, $indent . $extra ];
                }
                elsif ( $ref eq 'Tokens' ) {
                    push @new, [ $t, $newindent ];
                }
                elsif ( $ref eq 'Comment' ) {
                    push @new, $WS, [$t];
                }
                elsif ( $ref eq 'Op' ) {
                    push @new, $WS, [$t];
                }
                elsif ( $ref eq 'BoolOp' ) {    # Window Function
                    push @new, $NL, [$indent], [ $t, $indent ];
                }
                elsif ( $ref eq 'EOS' ) {       # End of Statement
                    push @new, [$t];
                }
                elsif ( $ref eq 'NotSQL' ) {    # generally a comment?
                    push @new, [$t];
                }
                else {
                    warn 'unhandled ' . $ref;
                    push @new, [ ' /* UNHAND ' . $ref . ' */ ' ];
                }
            }
        }
        elsif ( $ref eq 'List' ) {
            my @items = @{ $n->tokens };
            my $first = shift @items;
            push @new, $NL, [$indent], [ $first, $newindent ];
            my $COMMA = [','];
            foreach my $t (@items) {
                if ( ref($t) eq 'Expr' ) {
                    push @new, $COMMA, $NL, [$indent], [ $t, $newindent ];
                    $COMMA = [','];
                }
                elsif ( ref($t) eq 'Comment' ) {
                    push @new, $COMMA, [$t];
                    $COMMA = [''];
                }
                else {
                    warn "unhandled type " . ref($t);
                }
            }
        }
        elsif ( $ref eq 'Block' or $ref eq 'Function' ) {
            push @new, [ $n->val ];
            my @list    = @{ $n->tokens };
            my $i       = 0;
            my $complex = 0;
            foreach my $t (@list) {
                my $ref = ref($t);
                if ( $ref eq 'Expr' ) {
                    push @new, @list > 1 ? $WS : (), [ $t, $indent ];
                }
                elsif ( $ref eq 'List' ) {
                    push @new, [ $t, $indent ];
                    $complex++;
                }
                elsif ( $ref eq 'Keywords' ) {
                    push @new, [ $t, $indent ];
                }
                elsif ( $ref eq 'Op' ) {
                    push @new, [$t];
                }
                elsif ( $ref eq 'Comment' ) {
                    push @new, [ $t, $indent ];
                }
                elsif ( $ref eq 'Statement' ) {
                    $complex++;
                    push @new, $NL, [ $t, $indent ];
                }
                else {
                    warn 'unhandled Block/Function->' . $ref;
                    push @new, $WS, [ $t, $indent ];
                }
                $i++;
            }
            push @new,
              ( $complex ? ( $NL, [ $half . $indent =~ s/$extra//r ] ) : () ),
              [ $n->end ];
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
        elsif ( $ref eq 'EOS' ) {
            $clean .= $n->tokens->[0] . "\n";
        }
        elsif ( $ref eq 'Indent' ) {
            $lead_ws = $n->tokens->[0] =~ s/\t/$extra/gr;
        }
        elsif ( $ref eq 'Op' ) {
            $clean .= $n->tokens->[0];
        }
        elsif ( $ref eq 'NotSQL' ) {
            $clean .= join '', @{ $n->tokens };
        }
        else {
            # warn "unhandled X${n}X" unless;
            $clean .= $n;
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
        val    => $start,
        endval => $end,
        parent => $self->curr,
    );

    $self->add($block);
    $self->curr($block);
}

sub start_begin {
    my $self = shift;
    my $val  = shift;

    return $self->start_stmt($val) unless $self->in_statement;

    # must be a Trigger
    $self->add_keyword('begin');

    my $block = Block->new(
        val      => '',
        endval   => 'end',
        real_end => Keywords->new(
            val    => 'end',
            parent => $self->curr,
        ),
        parent => $self->curr,
    );

    $self->add($block);
    $self->curr($block);
    return;
}

sub start_case {
    my $self = shift;

    $self->start_block(
        Keywords->new(
            val    => 'case',
            parent => $self->curr,
        ),
        'end',
        Keywords->new(
            val    => 'end',
            parent => $self->curr,
        ),
    );
}

sub start_block {
    my $self     = shift;
    my $start    = shift;
    my $end      = shift;
    my $real_end = shift;

    unless ( $self->curr_isa('Expr') ) {
        my $e = Expr->new( parent => $self->curr );
        $self->add($e);
        $self->curr($e);
    }

    my $block = Block->new(
        val      => $start,
        endval   => $end,
        real_end => $real_end,
        parent   => $self->curr,
    );

    $self->add($block);
    $self->curr($block);
}

sub end_block {
    my $self = shift;
    my $end  = shift;
    if ( my $curr = $self->curr ) {
        while ( refaddr($curr) != refaddr($self) ) {
            if ( $curr->isa('Block') && lc( $curr->endval ) eq lc($end) ) {
                $self->curr( $curr->parent // $self );
                $self->latest( $self->curr );
                return;
            }
            $curr = $curr->parent // last;
        }
    }

    # did not find the start block!
    $self->add( Tokens->new(
        val    => $end . ' /* UNEXPECTED */',
        parent => $self->curr
    ) );
}

sub add_comment {
    my $self = shift;
    my $val  = shift;
    $val =~ s/\n$//;
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

    if ( not $self->in_statement ) {
        my $stmt = Statement->new( parent => $self->curr );
        $self->add($stmt);
        $self->curr($stmt);
    }

    # break out
    else {
        until (
                 $self->curr_isa('Statement')
              or $self->curr_isa('Block')
              or refaddr( $self->curr ) == refaddr($self)
          )
        {
            $self->curr( $self->curr->parent );
        }

        if ( $self->curr_isa('Block') ) {
            my $stmt = Statement->new( parent => $self->curr );
            $self->add($stmt);
            $self->curr($stmt);
        }
    }

    $self->add_keyword($val);
}

sub end_stmt {
    my $self = shift;
    my $val  = shift;

    if ( my $stmt = $self->in_statement ) {
        $stmt->add( EOS->new(
            val    => $val,
            parent => $self->curr,
        ) );
        $self->curr( $stmt->parent );
        return;
    }

    # warn or not?
    $self->add( EOS->new(
        val    => $val,
        parent => $self->curr,
    ) );
}

sub add_leadws {
    my $self = shift;
    my $val  = shift;

    return if $self->in_statement;
    $self->add( Indent->new( val => $val, parent => $self->curr ) );
}

sub add_ws {
    my $self = shift;
    my $val  = shift;

    return if $self->in_statement;
    $self->add( NotSQL->new(
        val    => $val =~ m/\n/ ? "\n" : $val,
        parent => $self->curr,
    ) );
}

sub add_op {
    my $self = shift;
    my $val  = shift;
    $self->add( Op->new( val => $val, parent => $self->curr, ) );
}

sub add_boolop {
    my $self = shift;
    my $val  = shift;
    $self->add( BoolOp->new( val => $val, parent => $self->curr, ) );
}

sub add_inline {
    my $self = shift;
    my $val  = shift;

    if ( $self->latest_isa('Inline') or $self->latest_isa('Keywords') ) {
        $self->latest->add($val);
        return;
    }

    unless ( $self->curr_isa('Expr') ) {
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

    warn "unknown SQL token '$val'" if $self->in_statement;
    $self->add( NotSQL->new(
        val    => $val,
        parent => $self->curr,
    ) );
}

### DO NOT EDIT BELOW! (generated by Class::Inline v0.0.1)
#<<<
  require Carp;require Scalar::Util;our@ATTRS_UNEX=(undef);sub new {my$class=
  shift;my$self={@_ ? @_ > 1 ? @_ : %{$_[0]}: ()};if (@ATTRS_UNEX){map {local
  $Carp::CarpLevel=$Carp::CarpLevel + 1;Carp::carp(
  "SQL::Tidy attribute '$_' unexpected");delete$self->{$_ }}sort grep {not
  exists$INLINE->{$_ }}keys %$self}else {@ATTRS_UNEX=map {delete$self->{$_ };
  $_}grep {not exists$INLINE->{$_ }}keys %$self}bless$self,ref$class || $class
  ;map {$self->{$_ }=eval {$INLINE->{$_ }->{'isa'}->($self->{$_ })};
  Carp::croak(qq{SQL::Tidy::$_ value invalid ($@)})if $@}grep {exists$self->{
  $_ }}'latest';map {Scalar::Util::weaken($self->{$_ })}grep {defined$self->{
  $_ }// undef}'latest';$self}sub curr {if (@_ > 1){$_[0]{'curr'}=$_[1];return
  $_[0]}$_[0]{'curr'}//= $INLINE->{'curr'}->{'default'}->($_[0])}sub indent {
  if (@_ > 1){$_[0]{'indent'}=$_[1];return $_[0]}$_[0]{'indent'}//= $INLINE->{
  'indent'}->{'default'}}sub latest {if (@_ > 1){$_[0]{'latest'}=eval {$INLINE
  ->{'latest'}->{'isa'}->($_[1])};Carp::croak(
  'invalid (SQL::Tidy::latest) value: '.$@)if $@;Scalar::Util::weaken($_[0]{
  'latest'})if defined $_[1];return $_[0]}$_[0]{'latest'}// undef}sub tokens {
  if (@_ > 1){$_[0]{'tokens'}=$_[1];return $_[0]}$_[0]{'tokens'}//= $INLINE->{
  'tokens'}->{'default'}->($_[0])}BEGIN{$INC{'SQL/Tidy.pm'}//= __FILE__}
#>>>
### DO NOT EDIT ABOVE! (generated by Class::Inline v0.0.1)

package Tokens;
use strict;
use warnings;
use Types::Standard (qw/Defined/);

our $INLINE = {
    CLASS  => { inc      => 1, },
    parent => { required => 1,       weaken   => 1, },
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

package Op;
use strict;
use warnings;
use parent 'Tokens';

package BoolOp;
use strict;
use warnings;
use parent 'Tokens';

package EOS;
use parent 'Tokens';

package Indent;
use parent 'Tokens';

package Expr;
use strict;
use warnings;

our $INLINE = {
    CLASS  => { inc      => 1, },
    parent => { required => 1,    weaken  => 1, },
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
    parent => { required => 1,    weaken  => 1, },
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
    CLASS    => { inc      => 1, },
    val      => { required => 1, },
    parent   => { required => 1, weaken => 1, },
    endval   => { required => 1, },
    real_end => { is       => 'rw' },
    tokens   => { is       => 'rw', default => sub { [] }, },
};

sub add {
    my $self = shift;
    my $val  = shift;
    push @{ $self->tokens }, $val;
}

sub end {
    my $self = shift;
    $self->real_end // $self->endval;
}

### DO NOT EDIT BELOW! (generated by Class::Inline v0.0.1)
#<<<
  require Carp;require Scalar::Util;our@ATTRS_UNEX=(undef);sub new {my$class=
  shift;my$self={@_ ? @_ > 1 ? @_ : %{$_[0]}: ()};map {local$Carp::CarpLevel=
  $Carp::CarpLevel + 1;Carp::croak("missing attribute Block::$_ is required")
  unless exists$self->{$_}}'endval','parent','val';if (@ATTRS_UNEX){map {local
  $Carp::CarpLevel=$Carp::CarpLevel + 1;Carp::carp(
  "Block attribute '$_' unexpected");delete$self->{$_ }}sort grep {not exists
  $INLINE->{$_ }}keys %$self}else {@ATTRS_UNEX=map {delete$self->{$_ };$_}grep
  {not exists$INLINE->{$_ }}keys %$self}bless$self,ref$class || $class;map {
  Scalar::Util::weaken($self->{$_ })}grep {defined$self->{$_ }// undef}
  'parent';$self}sub __ro {my (undef,undef,undef,$sub)=caller(1);local
  $Carp::CarpLevel=$Carp::CarpLevel + 1;Carp::croak(
  "attribute $sub is read-only (value: '" .($_[1]// 'undef')."')")}sub endval
  {$_[0]->__ro($_[1])if @_ > 1;$_[0]{'endval'}}sub parent {$_[0]->__ro($_[1])
  if @_ > 1;$_[0]{'parent'}}sub real_end {if (@_ > 1){$_[0]{'real_end'}=$_[1];
  return $_[0]}$_[0]{'real_end'}// undef}sub tokens {if (@_ > 1){$_[0]{
  'tokens'}=$_[1];return $_[0]}$_[0]{'tokens'}//= $INLINE->{'tokens'}->{
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

Copyright 2020-2021 Mark Lawrence <nomad@null.net>

