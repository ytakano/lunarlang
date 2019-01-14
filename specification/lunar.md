# The Specification of Lunar Language

## Type

```
$TYPESPEC := : $TYPE
```

### User Defined Type

```
$DEFTYPE := type $ID : { $INTYPE } | type $ID : $TYPE
$INTYPE := $T | $T `|` $SUM | $T , $PROD
$SUM := $T | $T `|` $SUM
$PROD := $T | $T , $PROD
$T := $ID $TYPESPEC? | $ID : { $INTYPE }
```

```
type foo : { bar : { a , b } |
             c : bool }

type foo : { bar : { a, b, c : bool} |
             fuz : {d : bool, e} }

type foo : { a, b }
```

## Function Definition

```
$DEFUN := fun $ID $TYPESPEC? ( $ARGS? ) { $EXPR }
$ARGS := $ARG | $ARG , $ARGS
$ARG := $ID $TYPESPEC?
```

## Expression

### If

### Let

### Function Call