#!/usr/bin/env perl
use strict;
use warnings;

package SQL::Tidy;
use Carp ();
use Scalar::Util 'refaddr';
use Types::Standard (qw/Maybe Object Defined/);
use Class::Inline {
    curr => {
        is      => 'rw',
        default => sub { $_[0] }
    },
    debug  => { is  => 'rw',           default => 0, },
    indent => { isa => Defined,        is      => 'rw', default => 4, },
    latest => { isa => Maybe [Object], is      => 'rw', weaken  => 1, },
    _parts => { is  => 'rw',           default => sub { [] }, },
};

our $VERSION = '0.0.1';

sub sqltidy {
    my $messy = shift // Carp::croak 'sqltidy: input undefined';
    my $sql   = __PACKAGE__->new(@_);
    $sql->parse($messy);
    $sql->tree2sql;
}

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
      RANGE ROWS GROUPS EXCLUDE RETURNING
      /
);

# should be seen as function not keyword:
# GROUP_CONCAT COALESCE REPLACE. The problem is with SELECT COALESCE(1,2)

my @INLINE = (
    qw/
      CONSTRAINT REFERENCES DATETIME DEFAULT
      FOREIGN INTEGER NOTNULL PRIMARY VARCHAR COLUMN ISNULL REGEXP UNIQUE
      CHECK INDEX MATCH BLOB CHAR DESC GLOB LIKE OVER TEXT ASC
      INT KEY NOT AS IN IS ON

      COLLATE
      CASCADE UNBOUNDED

      CURRENT_TIMESTAMP AUTOINCREMENT CURRENT_DATE CURRENT_TIME
      TRANSACTION DEFERRABLE EXCLUSIVE FOLLOWING GENERATED IMMEDIATE
      INITIALLY PRECEDING RECURSIVE TEMPORARY
      CONFLICT DATABASE DEFERRED DISTINCT RESTRICT BETWEEN
      CURRENT INDEXED NATURAL NOTHING
      TRIGGER VIRTUAL WITHOUT ACTION ALWAYS ESCAPE EXISTS
      FILTER IGNORE OTHERS STORED
      ABORT FIRST NULLS OUTER QUERY RAISE
      TABLE USING CAST EACH FAIL
      FULL INTO LAST PLAN TEMP TIES VIEW
      ALL END ROW BY IF NO OF TO
      MATERIALIZED

      /, 'ON DELETE', 'ON UPDATE', 'DEFAULT VALUES', 'OR IGNORE', 'OR REPLACE'
);

my @KEYWORDS = (
    qw/
      FROM INNER LEFT RIGHT JOIN WHERE GROUP HAVING UNION EXCEPT INTERSECT
      THEN ELSE CASE END SET FOR INSTEAD LIMIT OFFSET ADD DO RENAME
      CROSS WINDOW RETURNING AFTER BEFORE WHEN
      /
);

sub _lenorder {
    map    { s/ +/[\ \t]+/gr }           # more than single space for whitespace
      map  { s{([|/*+])}{\\$1}gr }       # escape regex chars
      sort { length $b <=> length $a }   # longest to shortest
      @_;
}

my $skeywords_re = join '|', _lenorder @STMT;
my $keywords_re  = join '|', _lenorder @KEYWORDS;
my $inline_re    = join '|', _lenorder @INLINE;
my $boolop_re    = join '|', _lenorder @BOOLOPS;
my $op_re        = join '|', _lenorder @OPERATORS;

my $ws_re       = qr/ [\ \t] /x;
my $leadws_re   = qr/ (?m) ^ $ws_re+ /sx;
my $wb_re       = qr/ \b | [\ \n \t] | $ /sx;
my $scomment_re = qr/ $ws_re* -- \N* \n? /sx;
my $comment_re  = qr/ \n? \/\* .*? (?: \*\/ | $ ) \n? /sx;
my $shell_re    = qr/ (?m) ^\. \N* \n? /mx;
my $passthru_re = qr/ $comment_re | $shell_re /x;
my $num_re      = qr/  (?: [0-9]+ (?: \. [0-9]*)? (?: e[+-]? [0-9]+ )? )
                     | (?: \. [0-9]+ (?: e[+-]? [0-9]+ )? ) /x;

my $string_re     = qr/ [xX]? ' (?: '' | [^'] )* ' /x;
my $word_re       = qr/ [_a-zA-Z] [_a-zA-Z0-9]* /x;
my $identifier_re = qr/ (?: " [^"]* " | :? $word_re ) /x;
my $column_re     = qr/ (?: $identifier_re \.){0,2} (?: $identifier_re | \* )/x;
my $token_re      = qr/ $column_re | $string_re | $op_re | $num_re | \?  /x;

our $__doc;

my $re = qr!
  (?:
    ($skeywords_re)$wb_re  (?{ $__doc->start_stmt($^N);             })
    | \(                   (?{ $__doc->start_block( qw/ ( ) / );    })
    | (\))                 (?{ $__doc->end_block( $^N );            })
    | (CASE)$wb_re         (?{ $__doc->start_case();                })
    | (END)$wb_re          (?{ $__doc->end_block( $^N );            })
    | ($inline_re)$wb_re   (?{ $__doc->add_inline($^N);             })
    | ($keywords_re)$wb_re (?{ $__doc->add_keyword($^N);            })

    # Needs to be before $op_re so that '-' does get matched early
    | ($scomment_re|$comment_re)       (?{ $__doc->add_comment($^N);            })
    | ($op_re)             (?{ $__doc->add_op($^N);                 })
    | ($boolop_re)$wb_re   (?{ $__doc->add_boolop($^N);             })
    | ($word_re)\s*\(      (?{ $__doc->start_function( $^N.'(', ')' ); })
    | ($passthru_re)       (?{ $__doc->add_rest($^N);               })
    | ($token_re)          (?{ $__doc->add_token($^N);              })
    | ,                    (?{ $__doc->make_list;                   })
    | ;$wb_re              (?{ $__doc->end_stmt(';');               })
    | ($leadws_re)         (?{ $__doc->add_leadws($^N);             })
    | (\s*\n | $ws_re+)    (?{ $__doc->add_ws($^N);                 })
    | \z                   (?{ $__doc->end_of_file();               })
    | (.)                  (?{ $__doc->add_rest($^N);               })
  )
!ix;

# (?{ warn "${^N} -> Cur: ".$__doc->curr." Last: ".$__doc->latest."\n" if length $^N })
#  (?{ print $__doc->tree2ast})

sub curr_is {
    my $self = shift;
    my $curr = ref $self->curr;
    0 < grep { $_ eq $curr } @_;
}

sub curr_isa {
    my $self = shift;
    my $curr = $self->curr;
    0 < grep { $curr->isa($_) } @_;
}

sub latest_is {
    my $self   = shift;
    my $latest = ref $self->latest;
    0 < grep { $_ eq $latest } @_;
}

sub latest_isa {
    my $self   = shift;
    my $latest = $self->latest;
    0 < grep { $latest->isa($_) } @_;
}

sub _reset {
    my $self = shift;
    $self->curr($self);
    $self->latest(undef);
    $self->_parts( [ SOF->new( parent => $self ) ] );
}

sub parts {
    my $self = shift;
    @{ $self->_parts };
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

    #$_->_dump(10) for $self->parts;
}

sub tree2ast {
    my $self  = shift;
    my $node  = shift // $self;
    my $clean = '-' x 70 . "\n";
    my $depth = '';
    my @parts = [ '', $node ], map { [ $depth . ' ', $_ ] } $node->parts;

    my $curr = $self->curr;

    while ( my $p = shift @parts ) {
        my ( $d, $part ) = @$p;
        my $ref = ref $part;
        if ($ref) {
            $clean .= '--| ' . $d . $ref;
            if ( $part->can('text') && length( my $text = $part->text ) ) {
                $clean .= ': ' . ( $text =~ s/\n/\\n/gr );
            }
            $clean .= ' (current)' if $part eq $curr;
            $clean .= ' (complex)' if $part->complex;
            $clean .= "\n";
            unshift @parts, map { [ $d . '  ', $_ ] } $part->parts;
        }
        else {
            warn 'XXXXXXXXXXX';    #.$t->[0] . $t->[1] . "\nXXXX";
        }
    }
    $clean . ( '-' x 70 ) . "\n";
}

my $D_ADD = bless {}, 'D_ADD';
my $D_SUB = bless {}, 'D_SUB';
my $NL    = bless {}, 'NL';

my %node_rules = ();

my %token_rules = (
    'BoolOp.Token'     => ' ',
    'Inline.Token'     => ' ',
    'Inline.Inline'    => ' ',
    'Keyword.Inline'   => ' ',
    'Keyword.Function' => ' ',
    'Keyword.Keyword'  => ' ',
    'Keyword.Token'    => ' ',

    #    'Inline.Comma'     => "\n",
    'Op.Token'     => ' ',
    'Op.Block'     => ' ',
    'Token.BoolOp' => ' ',

    #    'Token.EOS'       => '',
    'Token.Inline'  => ' ',
    'Token.Keyword' => "\n",
    'Token.Op'      => ' ',
    'Token.Token'   => ' ',
    'EOS.EOF'       => "\n",
    'EOS.Keyword'   => "\n\n",
    'SOF.Keyword'   => '',
);

sub tsql {
    my $self  = shift;
    my $debug = $self->debug;
    my $out   = Tidy->new( debug => $debug );

    foreach my $p ( $self->parts ) {
        $out->add( $self->tree2ast($p), 'Debug' )
          if $debug and 'Statement' eq ref $p;
        $p->add_to($out);
    }

    $out->out;
}

sub tsql2 {
    my $self  = shift;
    my $depth = '';
    my $debug = $self->debug;

    my $i          = 0;
    my $total      = $self->parts;
    my @parts      = map { $i++; [ $_, ref($_), $i, $total ] } $self->parts;
    my $prev       = shift @parts;    # start of file (SOF)
    my $prev_token = $prev;

    my @e = ('');

    #        push @e, $self->tree2ast() . "\n";

    while ( my $aref = shift @parts ) {
        my $n                      = $aref->[0];
        my $prev_combination       = $prev->[1] . '.' . $aref->[1];
        my $prev_token_combination = $prev_token->[1] . '.' . $aref->[1];

        push @e,
          ( length $e[-1] > 0 && $e[-1] !~ m/\n$/ ? "\n" : '' )
          . $self->tree2ast($n) . "\n"
          if $self->debug
          and $aref->[1] eq 'Statement'
          and ref( $n->parent ) eq 'SQL::Tidy';

        use Data::Dumper;

        sub d {
            my $t = shift;
            return 'undef' if not defined $t;
            my $var = Dumper($t) =~ s/(^\$VAR1 = )|(;\n$)//gr;
            $var =~ s/\n/\\n/g;
            $var =~ s/^'([^ ]+)'$/"'".colored(['yellow'],$1)."'"/e;
            $var =~ s/( )+/colored(['black on_yellow'],$1)/e;
            $var;
        }

        my $match  = $node_rules{$prev_combination}        // undef;
        my $tmatch = $token_rules{$prev_token_combination} // undef;

        use Term::ANSIColor 'colored';
        push @e, "/* $prev_combination: " . d($match) . " */\n"
          if $prev_combination ne $prev_token_combination
          and ( $debug == 2 and not defined $match )
          or $debug > 2;

        push @e, $match if defined $match;

        if ( $n->can('text') and length( my $text = $n->text ) ) {
            push @e,
              "/* (tokens) $prev_token_combination: " . d($tmatch) . ' */'
              if ( $debug == 1 and not defined $tmatch )
              or $debug > 1;

            if ( defined $tmatch ) {
                push @e, $tmatch;
            }
            push @e,
              colored( ['yellow'],
                $prev_token_combination =~ m/\.(Keyword|Inline|BoolOp)/
                ? uc($text)
                : $text );
            $prev_token = $aref;
        }

        $prev  = $aref;
        $i     = 0;
        $total = $n->parts;
        my @newparts = map { $i++; [ $_, ref($_), $i, $total ] } $n->parts;
        unshift @parts, @newparts;
    }

    join( '', @e );
}

sub tree2sql {
    my $self = shift;
    return $self->tsql;
    my $clean = '';
    my $extra = ' ' x $self->indent;
    my $half  = ' ' x ( $self->indent / 2 );
    my $NL    = ["\n"];
    my $WS    = [' '];
    my @parts = map { [$_] } $self->parts;

    my $lead_ws = '';
    my $tok     = 0;
    my $depth   = 0;

    while ( my $t = shift @parts ) {
        my ( $n, $ref, $indent ) =
          ( $t->[0], ref( $t->[0] ), $t->[1] // $lead_ws );
        my $newindent = $indent . $extra;
        my @new;
        $lead_ws = '';

        if ( not defined $n ) {
            warn "undefined token in the stream "
              . join( ',', map { $_->[0] } @parts );
        }
        elsif ( $ref eq '' ) {
            $clean .= $n;
        }
        elsif ( $ref eq 'Statement' ) {
            $clean .= $self->tree2ast($n) . "\n"
              if $self->debug and $n->parent eq $self;

            foreach my $t ( $n->parts ) {
                my ( $pos, $ref, $posiref, $total ) = $t->posiref;

                if ( $ref eq 'Keyword' ) {
                    push @new, $NL if $pos ne 'a';
                    push @new, [$indent], [$t];
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
                    push @new, $WS,    #( length $t->text ? $WS : () ),
                      [ $t, $indent . $extra ];
                }
                elsif ( $ref eq 'Token' ) {
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
            my @items = $n->parts;
            my $first = shift @items;
            push @new, $NL, [$indent], [ $first, $newindent ];
            my $COMMA = [','];
            foreach my $t (@items) {
                warn $t->posiref;
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
            push @new, $n->text;
            my @list    = $n->parts;
            my $complex = 0;
            foreach my $t (@list) {
                my ( $pos, $ref, $posiref, $total ) = $t->posiref;
                if ( $ref eq 'Token' ) {
                    push @new, [ $t, $indent ];
                }
                elsif ( $ref eq 'Expr' ) {
                    push @new, $WS unless $pos eq 'a';
                    push @new, [ $t, $indent ];
                }
                elsif ( $ref eq 'List' ) {
                    push @new, [ $t, $indent ];
                    $complex++;
                }
                elsif ( $ref eq 'Keyword' ) {
                    push @new, [ $t, $indent ];
                }

                #                elsif ( $ref eq 'Op' ) {
                #                    push @new, [$t];
                #                }
                elsif ( $ref eq 'Comment' ) {
                    push @new, [ $t, $indent ];
                }
                elsif ( $ref eq 'Statement' ) {
                    $complex++;
                    push @new, $NL, [ $t, $indent ];
                }
                else {
                    warn 'unhandled Block/Function->' . $ref;
                    push @new, ["/* unhandled Block/Function: $t */"], $WS,
                      [ $t, $indent ];
                }
            }
            push @new,
              ( $complex ? ( $NL, [ $half . $indent =~ s/$extra//r ] ) : () ),
              [ $n->end ];
        }
        elsif ( $ref eq 'Expr' ) {
            my $i = 1;
            foreach my $t ( $n->parts ) {
                if ( ref($t) eq 'Block' ) {
                    push @new, ( $i > 1 ? $WS : () ), [ $t, $newindent ];
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
        elsif ( $n->isa('Keyword') ) {
            $clean .= uc $n->text;
        }
        elsif ( $ref eq 'Indent' ) {
            $lead_ws = $n->text =~ s/\t/$extra/gr;
        }
        elsif ( $n->can('tok') ) {
            $clean .= $n->text;
            $clean .= "\n" if $ref eq 'EOS';
        }
        unshift @parts, @new;
    }
    return $clean;
}

sub add_part {
    my $self = shift;
    my $val  = shift;
    Carp::confess 'junk not a ref ' . $val unless ref $val;

    if ( refaddr( $self->curr ) != refaddr($self) ) {
        $self->curr->add_part($val);
    }
    else {
        push @{ $self->_parts }, $val;
    }

    $self->latest($val);
}

sub start_function {
    my $self  = shift;
    my $start = shift;
    my $end   = shift;

    if (    $self->latest_isa('Token')
        and $self->latest->parent eq $self->curr )
    {
        my $e = Expr->new( parent => $self->curr );
        $self->add_part($e);
        $self->curr($e);
    }

    my $block = Function->new(
        text   => $start,
        match  => $end,
        parent => $self->curr,
    );

    $self->add_part($block);
    $self->curr($block);
}

sub start_case {
    my $self = shift;

    #    unless ( $self->curr_is('Expr') ) {
    #        my $e = Expr->new( parent => $self->curr );
    #        $self->add_part($e);
    #        $self->curr($e);
    #    }

    my $block = Case->new(
        text   => 'case',
        match  => 'end',
        parent => $self->curr,
    );

    $self->add_part($block);
    $self->curr($block);
}

sub start_block {
    my $self  = shift;
    my $start = shift;
    my $end   = shift;

    unless ( $self->curr_is('Expr') ) {
        my $e = Expr->new( parent => $self->curr );
        $self->add_part($e);
        $self->curr($e);
    }

    my $block = Block->new(
        text   => $start,
        match  => $end,
        parent => $self->curr,
    );

    $self->add_part($block);
    $self->curr($block);
}

sub end_block {
    my $self = shift;
    my $end  = shift;
    if ( my $curr = $self->curr ) {
        while ( refaddr($curr) != refaddr($self) ) {
            if ( $curr->isa('Block') && lc( $curr->match ) eq lc($end) ) {
                my $blockend = ref($curr) . 'End';
                $curr->end( $blockend->new( parent => $curr, text => $end ) );
                $self->curr( $curr->parent // $self );
                $self->latest($curr);
                return;
            }
            $curr = $curr->parent // last;
        }
    }

    # did not find the start block!
    $self->add_part( Text->new(
        text   => $end . ' /* UNEXPECTED */',
        parent => $self->curr
    ) );
}

sub add_comment {
    my $self = shift;
    my $val  = shift;
    $val =~ s/\n$//;
    $self->add_part( Comment->new(
        text   => $val . "\n",
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

    unless ($self->latest_is('Keyword')
        and $self->latest->parent eq $self->curr )
    {

        # break out of a List or Expr
        until (
            $self->curr_isa(qw/Statement Block/)
              or refaddr( $self->curr ) == refaddr($self)
          )
        {
            $self->curr( $self->curr->parent );
        }
    }

    $self->add_part( Keyword->new(
        text   => $val,
        parent => $self->curr,
    ) );
}

sub start_stmt {
    my $self = shift;
    my $val  = shift;

    if ( not $self->in_statement ) {
        my $stmt = Statement->new( parent => $self->curr );
        $self->add_part($stmt);
        $self->curr($stmt);
        $self->add_keyword($val);
    }
    elsif ( $val =~ m/^begin$/i ) {

        #        $self->add_keyword($val);

        my $block = Block->new(
            text   => $val,
            match  => 'end',
            parent => $self->curr,
        );

        $self->add_part($block);
        $self->curr($block);
    }
    else {    # break out
        until (
                 $self->curr_is('Statement')
              or $self->curr_is('Block')
              or refaddr( $self->curr ) == refaddr($self)
          )
        {
            $self->curr( $self->curr->parent );
        }

        if ( $self->curr_is('Block') ) {
            my $stmt = Statement->new( parent => $self->curr );
            $self->add_part($stmt);
            $self->curr($stmt);
        }

        $self->add_keyword($val);
    }
}

sub end_stmt {
    my $self = shift;
    my $val  = shift;

    if ( my $stmt = $self->in_statement ) {
        $stmt->add_part( EOS->new(
            text   => $val,
            parent => $stmt,
        ) );
        $self->curr( $stmt->parent );
        return;
    }

    # warn or not?
    $self->add_part( EOS->new(
        text   => $val,
        parent => $self->curr,
    ) );
}

sub end_of_file {
    my $self = shift;
    $self->curr($self);

    $self->add_part( EOF->new(
        parent => $self,
    ) );
}

sub add_leadws {
    my $self = shift;
    my $val  = shift;

    return if $self->in_statement;
    $self->add_part( Indent->new(
        text   => $val,
        parent => $self->curr,
    ) );
}

sub add_ws {
    my $self = shift;
    my $val  = shift;

    return if $self->in_statement;
    $self->add_part( WS->new(
        text   => $val =~ m/\n/ ? "\n" : $val,
        parent => $self->curr,
    ) );
}

sub make_expr {
    my $self = shift;
    my $e    = Expr->new( parent => $self->curr );
    my $head = pop @{ $self->curr->_parts };
    $self->add_part($e);
    $self->curr($e);
    $self->add_part($head);
    $head->parent($e);    # TODO move this into add_part;
}

sub add_op {
    my $self = shift;
    my $val  = shift;

    if (    $self->latest_isa(qw/Token/)
        and $self->latest->parent eq $self->curr
        and not $self->curr_is('Expr') )
    {
        $self->make_expr;
    }

    $self->add_part( Op->new(
        text   => $val,
        parent => $self->curr,
    ) );
}

sub add_boolop {
    my $self = shift;
    my $val  = shift;

    if ( $self->curr_is('Expr') ) {
        $self->curr( $self->curr->parent );
    }

    $self->add_part( BoolOp->new(
        text   => $val,
        parent => $self->curr,
    ) );
}

sub add_inline {
    my $self = shift;
    my $val  = shift;

    # Need to stop Inline.Token combinations from add the Inline into
    # an Expr (which they need to be in other contexts)
    if (    $self->latest_is('Keyword')
        and $self->latest->parent eq $self->curr )
    {
        return $self->add_part( Keyword->new(
            text   => $val,
            parent => $self->curr,
        ) );
    }

    #    $self->make_expr unless $self->curr_is('Expr');

    if (    #   $self->latest_isa(qw/Token Inline/)
            #and
        $self->latest->parent eq $self->curr and not $self->curr_is('Expr')
      )
    {
        $self->make_expr;
    }

    #    unless (    $self->latest_is(qw/Keyword/)
    #        and $self->latest->parent eq $self->curr )
    #    {
    #        unless ( $self->in_statement ) {
    #            my $stmt = Statement->new( parent => $self->curr, );
    #            $self->add_part($stmt);
    #            $self->curr($stmt);
    #        }
    #
    #        my $e = Expr->new( parent => $self->curr );
    #        $self->add_part($e);
    #        $self->curr($e);
    #    }
    #    else {

    $self->add_part( Inline->new(
        text   => $val,
        parent => $self->curr,
    ) );
}

sub make_list {
    my $self   = shift;
    my $parent = $self->curr->parent;

    # If already in a list do nothing special
    if ( $self->curr_is(qw/List/) ) {

    }

    # Already in a list so just check for marking it as complex
    #    elsif ( ref($parent) eq 'List' ) {
    #        $parent->complex(1) if $self->curr->complex;
    #        $self->curr($parent);
    #        $self->latest($parent);
    #    }
    # Currently in an Expr, so put a new list in place of Expr and make
    # it the first item in the list
    elsif ( $self->curr_is(qw/Expr/) ) {
        my $head = pop @{ $parent->_parts };
        my $list = List->new( parent => $parent, complex => $head->complex );
        $parent->add_part($list);
        $list->add_part($head);    # TODO fix up parent
        $self->curr($list);
        $self->latest($head);
    }

    # A SELECT 1,2,3 situation or coalsece(id,other) where a comma
    # comes after a Token. So add the List and put the Token as the
    # first element like above.
    elsif ( $self->curr_is(qw/Block Function Statement/) ) {
        my $head = pop @{ $self->curr->_parts };
        my $list =
          List->new( parent => $self->curr, complex => $head->complex );
        $self->curr->add_part($list);
        $list->add_part($head);    # TODO fix up parent
        $self->curr($list);
        $self->latest($head);      # has it actually changed?
    }
    else {
        warn 'unhandled make_list with ' . $parent . $self->curr;
    }

    $self->add_part( Comma->new(
        text   => ',',
        parent => $self->curr,
    ) );
}

sub add_token {
    my $self = shift;
    my $val  = shift;

    if (    $self->latest_isa(qw/Token Inline/)
        and $self->latest->parent eq $self->curr
        and not $self->curr_is('Expr') )
    {
        $self->make_expr;
    }
    $self->add_part( Token->new(
        text   => $val,
        parent => $self->curr,
    ) );
}

sub add_rest {
    my $self = shift;
    my $val  = shift;

    warn "unknown SQL token '$val'" if $self->in_statement;
    $self->add_part( NotSQL->new(
        text   => $val,
        parent => $self->curr,
    ) );
}

package Tidy;
use Class::Inline {
    indent => {
        default => 0,
        is      => 'rw',
    },
    debug => {},
    prev  => {
        default => '',
        is      => 'rw',
    },
    max   => { default => 78, },
    lines => {
        default => sub { [''] },
        is      => 'rw',
    },
    rules => {
        default => sub { {
            'Block.Keyword'       => "\n",
            'BlockEnd.BoolOp'     => ' ',
            'BlockEnd.Inline'     => ' ',
            'BlockEnd.Keyword'    => "\n",
            'BoolOp.Block'        => ' ',
            'BoolOp.Function'     => ' ',
            'BoolOp.Inline'       => ' ',
            'BoolOp.Token'        => ' ',
            'Case.Keyword'        => "\n",
            'Case.Token'          => ' ',
            'CaseEnd.Keyword'     => "\n",
            'CaseEnd.Inline'      => ' ',
            'CaseEnd.Token'       => ' ',
            'Comma.Token'         => ' ',
            'EOS.Debug'           => "\n",
            'EOS.EOF'             => "\n",
            'EOS.Keyword'         => "\n\n",
            'Function.Token'      => '',
            'FunctionEnd.Inline'  => ' ',
            'FunctionEnd.Keyword' => "\n",
            'FunctionEnd.Op'      => ' ',
            'Inline.Block'        => ' ',
            'Inline.Keyword'      => "\n",
            'Inline.Function'     => ' ',
            'Inline.Inline'       => ' ',
            'Inline.Token'        => ' ',
            'Keyword.Case'        => ' ',
            'Keyword.Function'    => ' ',
            'Keyword.Inline'      => ' ',
            'Keyword.Keyword'     => ' ',
            'Keyword.Token'       => ' ',
            'Keyword.Block'       => ' ',
            'Op.Function'         => ' ',
            'Op.Token'            => ' ',
            'SOF.Keyword'         => '',
            'Token.BoolOp'        => ' ',
            'Token.Comma'         => '',
            'Token.FunctionEnd'   => '',
            'Token.Inline'        => ' ',
            'Token.Keyword'       => "\n",
            'Token.Op'            => ' ',
            'Token.Token'         => ' ',
        } },
    },
};

sub clone {
    my $self = shift;
    $self->new(
        indent => $self->indent,
        lines  => [ $self->lines->[-1] ],
        prev   => $self->prev,
        max    => $self->max,
        rules  => $self->rules,
    );
}

sub inc {
    my $self = shift;
    my $div  = shift // 1;
    $self->indent( $self->indent + 4 / $div );
}

sub dec {
    my $self = shift;
    my $div  = shift // 1;
    Carp::croak 'cannot dec below zero' unless $self->indent >= 4 / $div;
    $self->indent( $self->indent - 4 / $div );
}

sub add {
    my ( $self, $token, $type ) = @_;
    Carp::croak 'next not defined' unless defined $token;
    Carp::croak 'type not defined' unless defined $type;

    my $currline = pop @{ $self->lines };
    my $combi    = $self->prev . '.' . $type;

    use Data::Dumper;
    $Data::Dumper::Indent = 1;

    #warn Dumper [$combi, $token =~ s/\n/\\n/gr, $currline ];
    #    $currline .= ' /* '.$combi .' */ ';

    $token = uc $token
      if $type eq 'Keyword'
      or $type eq 'Inline'
      or $type eq 'Case'
      or $type eq 'BlockEnd'
      or $type eq 'BoolOp'
      or $type eq 'CaseEnd';
    warn $type if $token eq 'end';

    #warn 'CUR '.Dumper $currline;
    my $ind = ' ' x $self->indent;
    if ( $currline =~ m/\n\z/ ) {
        $token =~ s/\A(\N)/$ind$1/;
        $token =~ s/\n(\N)/\n$ind$1/g;

        #warn 'NEW1: '.Dumper $token =~ s/\n/\\n/gr;
        my @token = split /^/, $token;

        #warn 'NEW1: '.Dumper $token;
        push @{ $self->lines }, $currline, @token;
        $self->prev($type);
        return;
    }

    my $spacer =
        exists $self->rules->{$combi} ? $self->rules->{$combi}
      : $self->debug                  ? '/* ' . $combi . ' */'
      :                                 '';

    my $new = $currline . $spacer . $token;
    $new =~ s/\n(\N)/\n$ind$1/g;

    #warn 'NEW2: '.$self->indent .' '.Dumper $new =~ s/\n/\\n/gr;
    my @new = split /^/, $new;

    #warn 'NEW2: '.$self->indent .' '.Dumper \@new;
    push @{ $self->lines }, @new;
    $self->prev($type);

    #    if ( length($new) > $self->max and $token !~ m/^\n/ ) {
    #        $currline .= "\n";
    #        push @{ $self->lines }, ' ' x ( $self->indent + 2 );
    #    }
    #    else {
    #        $currline .= $spacer;
    #        $currline .= ( ' ' x $self->indent ) if $spacer =~ m/\n\z/;
    #    }
    #
    #    $currline .= $token;
    #    $self->prev($type);
    #
    #    warn scalar @{$self->lines} .': '.$token .': '. $currline;
}

sub txn {
    my $self   = shift;
    my $sub    = shift;
    my $lines  = $self->lines;
    my $indent = $self->indent;
    my $prev   = $self->prev;
    my $curr   = pop @{ $self->lines };

    $self->lines( [$curr] );
    my $ok = $sub->();

    if ($ok) {
        push @$lines, @{ $self->lines };
        $self->lines($lines);
    }
    else {
        push @{$lines}, $curr;
        $self->lines($lines);
        $self->indent($indent);
        $self->prev($prev);
    }

    $ok;
}

sub out {
    my $self = shift;
    join '', @{ $self->lines };
}

package Part;
use Class::Inline {
    complex => { is => 'rw' },
    parent  => {
        is       => 'rw',
        required => 1,
        weaken   => 1,
    },
    _parts => { default => sub { [] }, },
};

sub BUILD {
    my $self = shift;
    $self->make_complex if $self->complex;
}

sub make_complex {
    my $self   = shift;
    my $parent = $self->parent;
    while ( $parent && $parent->can('complex') ) {
        $parent->complex(1);
        $parent = $parent->parent;
    }
}

sub add_part {
    my $self = shift;
    my $val  = shift;
    push( @{ $self->_parts }, $val );
    $val->parent($self);
}

sub parts {
    @{ $_[0]->_parts };
}

sub add_to {
    my $self = shift;
    my $out  = shift;

    foreach my $p ( $self->parts ) {
        $p->add_to($out);
    }
}

package SOF;    # Start of File
use parent 'Part';

package Statement;
use parent 'Part';
use Class::Inline { complex => { default => 1, is => 'rw', }, };

package Indent;
use parent 'Part';

package Expr;
use parent 'Part';

package List;
use parent 'Part';

sub add_to {
    my $self = shift;
    my $out  = shift;

    unless ( $self->complex ) {
        my $ok = $out->txn( sub {
            foreach my $p ( $self->parts ) {
                $p->add_to($out);
            }
            return 1 == @{ $out->lines };
        } );

        return if $ok;
    }

    $out->add( "\n", 'NL' );
    $out->inc;
    my @parts = $self->parts;
    foreach my $p (@parts) {
        $p->add_to($out);
        $out->add( "\n", 'NL' ) if 'Comma' eq ref $p;
    }
    $out->dec;
    $out->add( "\n", 'NL' );
}

package EOF;
use parent 'Part';

package WS;
use parent 'Part';

package NL;
use parent 'Part';

package Text;
use parent 'Part';
use Types::Standard (qw/Defined/);
use Class::Inline {
    text => {
        isa      => Defined,
        required => 1,
    }
};

sub add_to {
    my $self = shift;
    my $out  = shift;

    $out->add( $self->text, ref $self );
    $self->SUPER::add_to($out);
}

package NotSQL;
use parent 'Text';

package Comment;
use parent 'Text';

package EOS;
use parent 'Text';

package Keyword;
use parent 'Text';

BEGIN { $INC{'Keyword.pm'} = __FILE__ }

package Inline;
use parent 'Keyword';

package BoolOp;
use parent 'Keyword';

package Comma;
use parent 'Text';

package BlockEnd;
use parent 'Text';

package CaseEnd;
use parent 'Text';

package FunctionEnd;
use parent 'Text';

package Token;
use parent 'Text';

BEGIN { $INC{'Token.pm'} = __FILE__ }

package Op;
use parent 'Token';

package Block;
use parent 'Token';
use Class::Inline {
    match => { required => 1, },
    end   => { is       => 'rw', },
};

sub add_to {
    my $self = shift;
    my $out  = shift;

    $out->add( $self->text, ref $self ) if length $self->text;

    #    $out->add( "\n", 'NL' );
    $out->inc;
    foreach my $p ( $self->parts ) {
        $p->add_to($out);
    }
    $out->dec(2);
    $out->add( "\n", 'NL' )  if $self->complex;
    $self->end->add_to($out) if $self->end;
    $out->dec(2);
}

package Case;
use parent 'Block';

sub BUILD {
    $_[0]->complex(1);
}

sub add_to {
    my $self = shift;
    my $out  = shift;

    $out->add( $self->text, ref $self );
    $out->inc;
    foreach my $p ( $self->parts ) {
        $p->add_to($out);
    }
    $out->add( "\n", 'NL' );
    $out->dec(2);
    $self->end->add_to($out) if $self->end;
    $out->dec(2);
}

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

