# The Specification of Lunar IR

Lunar IR is a low-level intermediate language to implement call-by-value functional programming languages.

## Top

```
$TOP := ($DEFUN | $DEFSTRUCT | $EXTERN)*
$EXPR := $LET | $ID | $DECIMAL | $FLOAT | $BOOL | $UTF8 | $APPLY | $VOID
```

## VOID

```
$VOID := ()
```

## Identifier

```
$ID := [^0-9$WHITESPACE][^$WHITESPACE]+
$WHITESPACE = space | tab | \r | \n | \r\n
```

## Decimal Number

```
$DECIMAL := [1-9][0-9]* | 0
```

## Floating Point Number

```
$FLOAT := $DECIMAL.[0-9]* $EXP? f?
$EXP := e $PLUSMINUS [0-9]+
$PLUSMINUS := + | -
```

## UTF-8 String

```
$UTF8 := " $CHAR* "
$ESCAPE := \a | \b | \f | \r | \n | \t | \v | \\ | \? | \' | \" | \0 | \UXXXXXXXX | \uXXXX
$CHAR := $ESCAPE | characters not in $ESCAPE
```

```
"Hello 世界!"
```

This expression returns a value whose type is `(ref utf8)`.

## Boolean

```
$BOOL := true | false
```

## Type

```
$TYPE := $SCALAR | (ref $REFTYPE)
$REFTYPE := $SCALAR | (ref $REFTYPE) | (struct $REFTYPE+) | $VEC | $FUN | $ID
$VEC := (vec $VECTYPE $DECIMAL?) | utf8
$VECTYPE := $SCALAR | (ref $REFTYPE) | (struct $REFTYPE+) | $ID
```

### Scalar Type

```
$SCALAR := bool | u64 | s64 | u32 | s32 | u16 | s16 | u8 | s8 | fp64 | fp32 | void
```

### Function Type

```
$FUN := (fun $TYPE ($TYPE*))
```

## Let

```
$LET := (let (($TYPE $ID $EXPR)+) $EXPR)
```

## Struct Definition

```
$DEFSTRUCT := (struct $ID ($STRUCTMEM $ID)+)
$STRUCTMEM := $SCALAR | (struct $STRUCTMEM+) | (ref $STRUCTMEM) | $VEC | $ID
```

## Function Definition

```
$DEFUN := (defun $ID $TYPE (($TYPE $ID)*) $EXPR)
```

## Function Prototype Definition

```
$EXTERN := (extern $ID $TYPE ($TYPE*))
```

## Function Apply

```
$APPLY := ($EXPR+)
```

### Basic Binary Operations

```
$OPS := ($OP $EXPR $EXPR+)
$OP := + | - | * | / | < | > | <= | >= | =
```

### If

```
(if $EXPR $EXPR $EXPR)
```

### Structure Creation

An instance of a structure type is created by ```(structure_type $EXPR+)``` and the expression returns
a value whose type is ```(ref structure_type)```.

Example:
```
(struct st (u64 foo))
(struct mystruct ((ref st) bar))
(defun fun void ((u32 foo))
    (let (((ref mystruct) x (mystruct (st 10))))
        ()))
```

### Vector Creation

An instance of a vector type is created by `(vec $TYPE number)` whose `number`
must be `u64` and the expression returns a value whose type is `(ref (vec $TYPE))`.

Example:
```
(defun fun void ()
    (let (((ref (vec u32)) x (vec u32 10)))
        ()))
```
This example creates a vector of `u32` whose size is 10.

### Element Access

```
(elm $EXPR $EXPR)
```

This function returns a reference of the element.
The first argument must be a value whose type is reference of vector or structure.

Example of vector type:
```
(defun fun (ref u32) (((ref (vec u32)) arg))
    (elm arg 1))
```

Example of structure type:
```
(struct st (u64 foo))
(defun fun (ref u64) (((ref st) arg))
    (elm arg 0))
```

### Load and Store

```
$LOAD := (laod $EXPR)
$STORE := (store $EXPR $EXPR)
```

### Yield

```
$YIELD := (yield)
```

### Channel

```
$MKCH := (mkch $TYPE)
$SENDCH := (sendch $ID $ID)
$RECVCH := (recvch $ID $ID)
```

- (sendch channel value)
  - send a value into the channel
- (recvch channel ref)
  - recv a value from the channel

mkch returns the identifier of a channel newly created.

sendch returns values as follows.

```
CH_SUCCESS = 0x00,
CH_FULL = 0x02,
CH_READ_CLOSED = 0x04,
CH_WRITE_CLOSED = 0x08,
```

recvch returns values as follows.

```
CH_SUCCESS = 0x00,
CH_EMPTY = 0x01,
CH_READ_CLOSED = 0x04,
CH_WRITE_CLOSED = 0x08,
```