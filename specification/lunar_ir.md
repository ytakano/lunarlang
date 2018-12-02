
$EXPR := $LET | $DEFUN | $APPLY

## Identifier

```
$ID := [^0-9$WHITESPACE][^$WHITESPACE]+
$WHITESPACE = space | tab | \r | \n | \r\n
```

## Type

```
$TYPE := $SCALAR
```

### Scalar Type

```
$SCALAR := bool | u64 | u32
```

## Let

```
$LET := (let (($TYPE $ID)+ $EXPR)+ $EXPR*)
```

## Function Definition

```
$DEFUN := (defun $ID? ($TYPE*) (($TYPE $ID)*) $EXPR*)
```

## Function Apply

```
$APPLY := ($ID $ID*)
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