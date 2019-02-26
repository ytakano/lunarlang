# The Specification of Lunar Language

```
$TOP := $CLASSDECL | $INST | $DEFUN
```

## Infix Operator

```
$INFIX := $INFIXCHAR+
$INFIXCHAR := + | - | < | > | / | % | : | & |
              * | ^ | @ | = | ! | ? | ~
```

## Reserved Words

```
$RESERVED := class | type | if | let | inst | where | fn |
             match | module | import | return | as |
             infix | $INFIX
```

## Identifier

```
$ID := [^0-9$WHITESPACE][^$WHITESPACE]+
$WHITESPACE := space | tab | \r | \n | \r\n
$NEWLINE := \r | \n | ;
$WHITESPACE2 := space | tab
$WHITESPACE3 := space | tab | \r | \n | \r\n | ;
$SEP := $WHITESPACE2* $NEWLINE+ $WHITESPACE3*
```

$ID must not be reserved words.

```
$IDS := $ID | $ID , $IDS
```

## Class

### Class Declaration

```
$CLASSDECL := class $ID $TVARKINDSP $PREDS? { $INTERFACES $WHITESPACE3* }
$INTERFACES := $INTERFACE | $INTERFACE $SEP $INTERFACES
$INTERFACE := fn $INTNAME ( $TYPES ) $TYPESPEC
$INTNAME := $ID | infix $INFIX
```

Example:
```
class ord<`a> where eq<`a> {
    fn infix < (`a, `a) : bool
    fn funcA (`a) : `a
}
```
This class definition define a class "ord" taking
a type variable "\`a" which is a member of the class "eq".

### Class Instance Declaration

```
$INST := inst $ID <$TYPES> $PREDS? { $DEFUNS }
```

Example:
```
inst ord<u32> {
    fn infix < (x, y) { ltU32(x, y) }
}

inst ord<either `a> where ord<'a> {
    fn infix < (x, y) {
        match (x, y) {
        just a, just b:
            lt(a, b)
        _:
            false
        }
    }
}
```

### Predicate

A predicate asserts <$TYPES> is a member of the class named by $ID.
```
$PREDS := where $PREDS_
$PREDS_ := $PRED | $PRED, $PRED
$PRED := $ID <$TYPES>
```

Example:
```
where ord<`a>, eq<`b>
```
This predicate assert \`a and \`b are members of ord and eq classes,
respectively.

## Type

### Kind

```
$KIND := $STAR | $STAR -> $KIND
$STAR := *
```

### Type Variable

The leading character of a type variable must be ` (backslash).
```
$TVAR := `$ID
$TVARKIND := `$ID | `$ID :: $KIND
$TVARKINDS := $TVARKIND | $TVARKIND , $TVARKINDS
$TVARS := <$TVARKINDS>
```

### Type

```
$TYPE := $IDTVAR <$TYPES>? | fn ( $TYPES? ) $TYPESPEC | ( $TYPES? )
$IDTVAR := $ID | $TVAR
$TYPES := $TYPE | $TYPE , $TYPES
```

### Type Specifier

```
$TYPESPEC := : $TYPE
```

### User Defined Type

```
$DEFTYPE := type $ID $TVARKINDSP? { $INTYPE }
$INTYPE := $SUM | $PROD
$SUM := $SUMTYPE | $SUMTYPE "|" $SUM
$SUMTYPE := $ID | $ID $TYPESPEC | $ID : { $INTYPE }
$PROD := $PRODTYPE | $PRODTYPE , $PROD
$PRODTYPE := $ID $TYPESPEC | $ID : { $INTYPE }
```

```
type foo { x | y }

type foo <`a, `b> { x : `a , y : `b }

type foo { bar : { x | y } |
           z : bool }
```

## Function Definition

```
$DEFUN := fn $ID ( $ARGS? ) $RETTYPE? $PREDS? { $EXPRS }
$DEFUNS := $DEFUN | $DEFUNS $SEP $DEFUN
$ARGS := $ARG | $ARG , $ARGS
$ARG := $ID $TYPESPEC?
$RETTYPE := : $TYPE
```

```
fn myfun (x : `a, y : `b) : `a where num<`a>, bool<`b> { x }
```

## Expression

```
$EXPR0 := $ID | $IF | $LET | ( $EXPR , ) | ( $EXPR ) | ( $EXPRS_? ) |
          { $DICT } | { $EXPRS } | [ $EXPRS_? ] | $LITERAL
$EXPR := $EXPR0 $EXPR'
$EXPR' := ∅ | . $ID $EXPR' | $INFIX $EXPR $EXPR' | [ $EXPR ] $EXPR' |
          $APPLY $EXPR'
$EXPRS := $EXPR | $EXPR $SEP $EXPR
$EXPRS_ := $EXPR | $EXPR , $EXPR
$LITERAL := $STR | $DECIMAL | $FLOAT
```

### Apply

```
$APPLY := ( $EXPRS_ ) | ( $EXPRS_ , $NAMEDS ) | ( $NAMEDS )
$NAMED := $ID : $EXPR
$NAMEDS := $NAMED | $NAMED $NAMED
```

### If

```
$IF := if $EXPR { $EXPRS } $ELSE?
$ELSE := elif $EXPR { $EXPRS } $ELSE | else { $EXPRS }
```

### Let

```
$LET := let $DEFVARS $IN?
$DEFVAR := $ID = $EXPR $TYPESPEC?
$DEFVARS := $DEFVAR | $DEFVAR , $DEFVARS
$IN := in { $EXPRS }
```

### Dict

```
$DICT := $DICTELM | $DICTELM , $DICT
$DICTELM := $EXPR : $EXPR
```

## String Literal

```
$STR := " $CHAR* "
$ESCAPE := \a | \b | \f | \r | \n | \t | \v | \\ | \? | \' | \" | \0 | \UXXXXXXXX | \uXXXX
$CHAR := $ESCAPE | characters not in $ESCAPE
```

## Decimal Number Literal

```
$DECIMAL := [1-9][0-9]* | 0
```

## Floating Point Number Literal

```
$FLOAT := $DECIMAL.[0-9]* $EXP? f?
$EXP := e $PLUSMINUS [0-9]+
$PLUSMINUS := + | -
```