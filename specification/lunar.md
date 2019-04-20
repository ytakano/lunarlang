# The Specification of Lunar Language

```
$TOP := $CLASSDECL | $INST | $DEFUN
```

## Infix Operator

```
$INFIX := $INFIXCHAR+
$INFIXCHAR := + | - | < | > | / | % | : | & |
              * | ^ | @ | = | ! | ? | ~ | .
```

| priority | operators
|----------|-----------
| 20       | (  )
| 19       | .
| 14       | * / %
| 13       | + -
| 12       | << >>
| 11       | < > <= >=
| 10       | == !=

## Reserved Words

```
$RESERVED := class | type | if | let | instance | require | func |
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
$CLASSDECL := class $ID $TVARS $PREDS? { $INTERFACES $WHITESPACE3* }
$INTERFACES := $INTERFACE | $INTERFACE $SEP $INTERFACES
$INTERFACE := func $INTNAME ( $TYPES ) $TYPESPEC
$INTNAME := $ID | infix $INFIX
```

Example:
```
class ord<`a> require eq<`a> {
    func infix < (`a, `a) : bool
    func funcA (`a) : `a
}
```
This class definition define a class "ord" taking
a type variable "\`a" which is a member of the class "eq".

### Class Instance Declaration

```
$INST := instance $PRED $PREDS? { $DEFUNS }
```

Example:
```
instance ord<u32> {
    func infix < (x, y) { ltU32(x, y) }
}

instance ord<either `a> require ord<'a> {
    func infix < (x, y) {
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
$PREDS := require $PREDS_
$PREDS_ := $PRED | $PRED, $PRED
$PRED := $ID <$TYPES>
```

Example:
```
require ord<`a>, eq<`b>
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
$TYPE := $IDTVAR <$TYPES>? | func ( $TYPES? ) $TYPESPEC | ( $TYPES? ) | [ $TYPE $ARRNUM? ]
$ARRNUM := * $EXPR
$IDTVAR := $ID | $TVAR
$TYPES := $TYPE | $TYPE , $TYPES
```

### Type Specifier

```
$TYPESPEC := : $TYPE
```

### Struct and Union

```
$STRUCT := struct $ID $TVARS? $PREDS? { $PROD }
$PROD := $PRODTYPE | $PRODTYPE $SEP $PROD
$PRODTYPE := $ID $TYPESPEC | $ID : struct { $PROD } | $ID : union { $SUM }
```

```
$UNION := struct $ID $TVARS? $PREDS? { $SUM }
$SUM := $SUMTYPE | $SUMTYPE $SEP $SUM
$SUMTYPE := $ID | $ID $TYPESPEC | $ID : struct { $PROD } | $ID : union { $SUM }
```

## Function Definition

```
$DEFUN := func $ID ( $ARGS? ) $RETTYPE? $PREDS? { $EXPRS }
$DEFUNS := $DEFUN | $DEFUNS $SEP $DEFUN
$ARGS := $ARG | $ARG , $ARGS
$ARG := $ID $TYPESPEC?
$RETTYPE := : $TYPE
```

```
func myfun (x : `a, y : `b) : `a require num<`a>, bool<`b> { x }
```

## Expression

```
$EXPR := $EXPR0 $EXPR' | $EXPR0 $EXPR' $INFIX+ $EXPR
$EXPR0 := $ID | $IF | $LET | ( $EXPR , ) | ( $EXPR ) | ( $EXPRS_? ) |
          { $DICT } | { $EXPRS } | [ $EXPRS_? ] | $LITERAL
$EXPR' := ∅ | [ $EXPR ] $EXPR' | $APPLY $EXPR'
$EXPRS := $EXPR | $EXPR $SEP $EXPR
$EXPRS_ := $EXPR | $EXPR , $EXPR
$LITERAL := $STR | $DECIMAL | $FLOAT
```

### Apply

```
$APPLY := ( $EXPRS_? )
```

TODO: named arguments, optional

### If

```
$IF := if $EXPR { $EXPRS } $ELSE?
$ELSE := elif $EXPR { $EXPRS } $ELSE | else { $EXPRS }
```

### Let

```
$LET := let $DEFVARS $IN?
$DEFVAR := $ID $TYPESPEC? = $EXPR
$DEFVARS := $DEFVAR | $DEFVAR , $DEFVARS
$IN := in $EXPR
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