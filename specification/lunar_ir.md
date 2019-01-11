
```
$TOP := ($DEFUN | $DEFSTRUCT | $EXTERN)*
$EXPR := $LET | $ID | $DECIMAL | $FLOAT | $APPLY | $VOID
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

## Type

```
$TYPE := $SCALAR | (ref $REFTYPE) | (fun $TYPE ($TYPE*))
$REFTYPE := $SCALAR | (struct $REFTYPE+) | (ref $REFTYPE) | $VEC | $ID
$VEC := (vec $TYPE $DECIMAL?)
```

### Scalar Type

```
$SCALAR := bool | u64 | s64 | u32 | s32 | u16 | s16 | u8 | s8 | fp64 | fp32 | utf8 | void
```

## UTF8

```
$UTF8 = utf8
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

### Structure Creation

An instance of a structure type is created by ```(structure_type $EXPR+)``` and the expression returns
a ```(ref structure_type)``` type.

Example:
```
(struct st (u64 foo))
(struct mystruct ((ref st) bar))
(defun fun void ((u32 foo))
    (let (((ref mystruct) x (mystruct (st 10))))
        ()))
```

### Vector Creation

An instance of a vector type is created by ```(vec $TYPE number)``` whose number variable must be ```u64``` and the expression returns a ```(vec $TYPE)``` type.

Example:
```
(defun fun void ()
    (let (((vec u32) x (vec u32 10)))
        ()))
```
This example creates a u32 vector whose size is 10.

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