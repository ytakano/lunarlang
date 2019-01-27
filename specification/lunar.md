# The Specification of Lunar Language

## Infix Operator

```
$INFIX := + | - | = | < | > | != | >>=
```

## Reserved Words

```
$RESERVED := class | type | if | let | inst | where | fn | return | $INFIX
```

## Identifier

```
$ID := [^0-9$WHITESPACE][^$WHITESPACE]+
$WHITESPACE = space | tab | \r | \n | \r\n
$NEWLINE = \r | \n | ;
```

$ID must not be reserved words.

```
$IDS := $ID | $ID , $IDS
```

## Class

### Class Declaration

```
$CLASSDECL := class $ID $TVARSP $PREDS? { $FUNHEADS }
$FUNHEADS := $FUNHEAD | $FUNHEAD $NEWLINE $FUNHEAD
```

Example:
```
class ord<`a> where eq<`a> {
    infix < (x : `a, y : `a) -> bool
}
```
This class definition define a class "ord" taking
a type variable "\`a" which is a member of the class "eq".

### Class Instance Declaration

```
$INST := inst $ID <$TYPES> $PREDS? { $EXPRS }
```

Example:
```
inst ord<u32> {
    infix < (x, y) { ltU32(x, y) }
}

inst ord<either `a> where ord<'a> {
    infix < (x, y) {
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

### Type Variable

The leading character of a type variable must be ` (backslash).
```
$TVAR := `$ID
$TVARS := $TVAR | $TVAR , $TVARS
$TVARSP := <$TVARS>
```

### Type

```
$TYPE := $ID <$TYPES>? | $TVAR
$TYPES := $TYPE | $TYPE , $TYPES
```

### Type Specifier

```
$TYPESPEC := : $TYPE
```

### User Defined Type

```
$DEFTYPE := type $ID $TVARSP? { $INTYPE }
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
$DEFUN := $FUNHEAD { $EXPR }
$FUNHEAD := fn $ID ( $ARGS? ) $RETTYPE $PREDS
$ARGS := $ARG | $ARG , $ARGS
$ARG := $ID $TYPESPEC?
$RETTYPE := -> $TYPE
```

```
fn myfun (x : `a, y : `b) -> `a where num<`a>, bool<`b> { x }
```

## Expression

```
$EXPR := $ID
$EXPRS := $EXPR | $EXPR $NEWLINE $EXPR
```

### If

### Let

### Function Call