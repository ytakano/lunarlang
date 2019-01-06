
```
$TOP := ($DEFUN | $DEFSTRUCT | $EXTERN)*
$EXPR := $LET | $ID | DECIMAL | $APPLY | $VOID
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
$DECIMAL := [1-9][0-9]*
```

## Type

```
$TYPE := $SCALAR | (ref $REFTYPE) | (fun $TYPE ($TYPE*))
$REFTYPE := $SCALAR | (struct $REFTYPE+) | (ref $REFTYPE) | $ID
```

### Scalar Type

```
$SCALAR := bool | u64 | s64 | u32 | s32 | u16 | s16 | u8 | s8 | fp64 | fp32 | void
```

## Let

```
$LET := (let (($TYPE $ID $EXPR)+) $EXPR)
```

## Struct Definition

```
$DEFSTRUCT := (struct $ID ($STRUCTMEM $ID)+)
$STRUCTMEM := $SCALAR | $ID | (struct $STRUCTMEM) | (ref $STRUCTMEM)
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

### Reference

```
$REF := (ref $EXPR)
```

$EXPR must be structure type

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