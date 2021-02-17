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
      RANGE ROWS GROUPS EXCLUDE WHEN
      /
);

# should be seen as function not keyword:
# GROUP_CONCAT COALESCE REPLACE. The problem is with SELECT COALESCE(1,2)

my @INLINE = (
    qw/
      CONSTRAINT REFERENCES DATETIME DEFAULT
      FOREIGN INTEGER NOTNULL PRIMARY VARCHAR COLUMN ISNULL REGEXP UNIQUE
      CHECK INDEX MATCH BLOB CHAR DESC GLOB LIKE OVER TEXT ASC
      INT KEY NOT AS IN IS

      COLLATE
      CASCADE UNBOUNDED

      CURRENT_TIMESTAMP AUTOINCREMENT CURRENT_DATE CURRENT_TIME
      TRANSACTION DEFERRABLE EXCLUSIVE FOLLOWING GENERATED IMMEDIATE
      INITIALLY PRECEDING RECURSIVE TEMPORARY
      CONFLICT DATABASE DEFERRED DISTINCT RESTRICT BETWEEN
      CURRENT INDEXED NATURAL NOTHING
      TRIGGER VIRTUAL WITHOUT ACTION ALWAYS BEFORE ESCAPE EXISTS
      FILTER IGNORE OTHERS STORED
      ABORT FIRST NULLS OUTER QUERY RAISE
      TABLE USING CAST EACH FAIL
      FULL INTO LAST PLAN TEMP TIES VIEW
      ALL END ROW BY IF NO OF TO

      /, 'ON DELETE', 'ON UPDATE', 'DEFAULT VALUES', 'OR IGNORE', 'OR REPLACE'
);

my @KEYWORDS = (
    qw/
      FROM INNER LEFT RIGHT JOIN WHERE GROUP HAVING UNION EXCEPT INTERSECT
      THEN ELSE CASE END ON SET FOR INSTEAD LIMIT OFFSET ADD DO RENAME
      CROSS WINDOW RETURNING AFTER
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
    | ($token_re)          (?{ $__doc->add_expr($^N);               })
    | ,                    (?{ $__doc->make_list;                   })
    | ;$wb_re              (?{ $__doc->end_stmt(';');               })
    | ($leadws_re)         (?{ $__doc->add_leadws($^N);             })
    | (\s*\n | $ws_re+)    (?{ $__doc->add_ws($^N);                 })
    | \z                   (?{ $__doc->end_of_file();               })
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

    while ( my $p = shift @parts ) {
        my ( $d, $part ) = @$p;
        my $ref = ref $part;
        if ($ref) {
            $clean .= '--| ' . $d . $ref;
            if ( $part->can('tok') && length( my $tok = $part->tok ) ) {
                $clean .= ': ' . ( $tok =~ s/\n/\\n/gr );
            }
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
    'BoolOp.Token'    => ' ',
    'Inline.Token'    => ' ',
    'Keyword.Inline'  => ' ',
    'Keyword.Keyword' => ' ',
    'Keyword.Token'   => ' ',
    'Op.Token'        => ' ',
    'Token.BoolOp'    => ' ',

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

        if ( $n->can('tok') and length( my $tok = $n->tok ) ) {
            push @e,
              "/* (tokens) $prev_token_combination: " . d($tmatch) . ' */'
              if ( $debug == 1 and not defined $tmatch )
              or $debug > 1;

            if ( defined $tmatch ) {
                push @e, $tmatch;
            }
            push @e, colored( ['yellow'], $tok );
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
                    push @new, $WS,    #( length $t->tok ? $WS : () ),
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
            push @new, $n->tokens;
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
            $clean .= uc $n->tok;
        }
        elsif ( $ref eq 'Indent' ) {
            $lead_ws = $n->tok =~ s/\t/$extra/gr;
        }
        elsif ( $n->can('tok') ) {
            $clean .= $n->tok;
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

    unless ( $self->curr_isa('Expr') ) {
        my $e = Expr->new( parent => $self->curr );
        $self->add_part($e);
        $self->curr($e);
    }

    my $block = Function->new(
        tokens => [$start],
        match  => $end,
        end    => Token->new(
            tokens => [$end],
            parent => $self->curr,
        ),
        parent => $self->curr,
    );

    $self->add_part($block);
    $self->curr($block);
}

sub start_case {
    my $self = shift;

    unless ( $self->curr_isa('Expr') ) {
        my $e = Expr->new( parent => $self->curr );
        $self->add_part($e);
        $self->curr($e);
    }

    my $block = Block->new(
        tokens => ['case'],
        match  => 'end',
        end    => Keyword->new(
            tokens => ['end'],
            parent => $self->curr,    # TODO change to add_end()
        ),
        parent => $self->curr,
    );

    #    $block->add_part( Keyword->new(
    #        tokens => ['case'],
    #        parent => $block,
    #    ) );

    $self->add_part($block);
    $self->curr($block);
}

sub start_block {
    my $self  = shift;
    my $start = shift;
    my $end   = shift;

    unless ( $self->curr_isa('Expr') ) {
        my $e = Expr->new( parent => $self->curr );
        $self->add_part($e);
        $self->curr($e);
    }

    my $block = Block->new(
        tokens => [$start],
        match  => $end,
        end    => Token->new(
            tokens => [$end],
            parent => $self->curr,
        ),
        parent => $self->curr,
    );

    #    $block->add_part( Token->new(
    #        tokens => [$start],
    #        parent => $block,
    #    ) );

    $self->add_part($block);
    $self->curr($block);
}

sub end_block {
    my $self = shift;
    my $end  = shift;
    if ( my $curr = $self->curr ) {
        while ( refaddr($curr) != refaddr($self) ) {
            if ( $curr->isa('Block') && lc( $curr->match ) eq lc($end) ) {
                $self->curr( $curr->parent // $self );
                $self->latest( $self->curr );
                return;
            }
            $curr = $curr->parent // last;
        }
    }

    # did not find the start block!
    $self->add_part( Part->new(
        tokens => [ $end . ' /* UNEXPECTED */' ],
        parent => $self->curr
    ) );
}

sub add_comment {
    my $self = shift;
    my $val  = shift;
    $val =~ s/\n$//;
    $self->add_part( Comment->new(
        tokens => [ $val . "\n" ],
        parent => $self->curr,
    ) );
}

sub add_token {
    my $self = shift;
    my $val  = shift;
    $self->add_part( Part->new(
        tokens => [$val],
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

    if (    $self->latest_isa('Keyword')
        and $self->latest->parent eq $self->curr )
    {
        $self->latest->add_token($val);
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

    $self->add_part( Keyword->new(
        tokens => [$val],
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
            match => 'end',
            end   => Keyword->new(
                tokens => ['end'],
                parent => $self->curr,
            ),
            parent => $self->curr,
        );

        $block->add_part( Keyword->new(
            tokens => [$val],
            parent => $block,
        ) );

        $self->add_part($block);
        $self->curr($block);
    }
    else {    # break out
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
            tokens => [$val],
            parent => $stmt,
        ) );
        $self->curr( $stmt->parent );
        return;
    }

    # warn or not?
    $self->add_part( EOS->new(
        tokens => [$val],
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
        tokens => [$val],
        parent => $self->curr,
    ) );
}

sub add_ws {
    my $self = shift;
    my $val  = shift;

    return if $self->in_statement;
    $self->add_part( WS->new(
        tokens => [ $val =~ m/\n/ ? "\n" : $val ],
        parent => $self->curr,
    ) );
}

sub add_op {
    my $self = shift;
    my $val  = shift;
    $self->add_part( Op->new(
        tokens => [$val],
        parent => $self->curr,
    ) );
}

sub add_boolop {
    my $self = shift;
    my $val  = shift;
    $self->add_part( BoolOp->new(
        tokens => [$val],
        parent => $self->curr,
    ) );
}

sub add_inline {
    my $self = shift;
    my $val  = shift;

    if ( $self->latest_isa('Inline') or $self->latest_isa('Keyword') ) {
        $self->latest->add_token($val);
        return;
    }

    unless ( $self->curr_isa('Expr') ) {
        unless ( $self->in_statement ) {
            my $stmt = Statement->new( parent => $self->curr, );
            $self->add_part($stmt);
            $self->curr($stmt);
        }

        my $e = Expr->new( parent => $self->curr );
        $self->add_part($e);
        $self->curr($e);
    }

    $self->add_part( Inline->new(
        tokens => [$val],
        parent => $self->curr,
    ) );
}

sub make_list {
    my $self   = shift;
    my $parent = $self->curr->parent;

    if ( ref($parent) eq 'List' ) {
        $self->curr($parent);
        $self->latest($parent);
    }
    elsif ( $self->curr_isa('Expr') ) {
        my $bottom = delete $parent->_parts->[-1];
        my $middle = List->new( parent => $parent );
        $parent->add_part($middle);
        $middle->add_part($bottom);
        $self->curr($middle);
        $self->latest($middle);
    }
    elsif ( $self->curr_isa('Statement') ) {
        my $bottom = delete $self->curr->_parts->[-1];
        my $list   = List->new( parent => $self->curr );
        $self->add_part($list);
        $list->add_part($bottom);
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
        $self->add_part($e);
        $self->curr($e);
    }
    $self->add_part( Token->new(
        tokens => [$val],
        parent => $self->curr,
    ) );
}

sub add_rest {
    my $self = shift;
    my $val  = shift;

    warn "unknown SQL token '$val'" if $self->in_statement;
    $self->add_part( NotSQL->new(
        tokens => [$val],
        parent => $self->curr,
    ) );
}

package Part;
use Class::Inline {
    parent => {
        is       => 'rw',
        required => 1,
        weaken   => 1,
    },
    _parts => { default => sub { [] }, },
};

sub add_part {
    my $self = shift;
    my $val  = shift;
    push( @{ $self->_parts }, $val );
    $val->parent($self);
}

sub parts {
    @{ $_[0]->_parts };
}

package SOF;    # Start of File
use parent 'Part';

package Statement;
use parent 'Part';

package Indent;
use parent 'Part';

package Expr;
use parent 'Part';

package List;
use parent 'Part';

package EOF;
use parent 'Part';

package WS;
use parent 'Part';

package NL;
use parent 'Part';

package Token;
use parent 'Part';
use Class::Inline { tokens => { default => sub { [] }, }, };

sub add_token {
    my $self = shift;
    my $val  = shift;
    push( @{ $self->tokens }, $val );
}

sub tok {
    my $self = shift;
    join( ' ', @{ $self->tokens } );
}

package NotSQL;
use parent 'Token';

sub tok {
    my $self = shift;
    join '', @{ $self->tokens };
}

package Comment;
use parent 'Token';

package Op;
use parent 'Token';

package EOS;
use parent 'Token';

package Keyword;
use parent 'Token';
BEGIN { $INC{'Keyword.pm'} = __FILE__ }

package Inline;
use parent 'Keyword';

package BoolOp;
use parent 'Keyword';

package Block;
use parent 'Token';
use Class::Inline {
    match => { required => 1, },
    end   => { required => 1, },
};

sub parts {
    my $self = shift;
    $self->SUPER::parts, $self->end;
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

