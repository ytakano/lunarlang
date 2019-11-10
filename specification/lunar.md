# The Specification of Lunar Language

```
$TOP  := $TOP2 $TOP | ∅
$TOP2 := $CLASSDECL | $INST | $DEFUN | $IMPORT
```

## Prefix Poerator

```
$PREFIX := -
```


## Infix Operator

```
$INFIX     := $INFIXCHAR+
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
| 3        | :=

## Reserved Words

```
$RESERVED := true | false | void |
             class | instance | if | elif | else |
             let | func | require |
             match | in | import | as | here |
             prefix | infix | shared | uniq |
             struct | data
```

## Identifier

```
$ID          := [^0-9$WHITESPACE][^$WHITESPACE]+
$WHITESPACE  := space | tab | \r | \n | \r\n
$SEP         := $SEP*
$SEP'        := ; | $WHITESPACE
```

$ID must not be reserved words.

```
$DOTID = $ID | $ID . $DOTID
```

## Import

```
$IMPORT := import $DOTID $HEREAS?
$HEREAS := here | $AS
$AS := as $ID
```

## Class

### Class Declaration

```
$CLASSDECL  := class $ID < $TVARKIND > $PREDS? { $INTERFACES }
$INTERFACES := $INTERFACE | $INTERFACE $SEP $INTERFACES
$INTERFACE  := $INTNAMES :: func ( $QTYPES? ) $RETTYPE
$INTNAMES   := $INTNAME | $INTNAME , $INTNAMES
$INTNAME    := $ID | infix $INFIX | prefix $PREFIX
```

Example:

```
class ord<`a> require eq<`a> {
    funcA, funcB :: func(`a) -> `a
    infix <, infix > :: func(`a, `a) -> bool
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

instance ord<either<`a>> require ord<`a> {
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

A predicate asserts <$TYPE> is a member of the class named by $ID.

```
$PREDS  := require $PREDS_
$PREDS_ := $PRED | $PRED, $PRED
$PRED   := $DOTID <$QTYPE>
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

The leading character of a type variable must be ` (backquote).

```
$TVAR      := `$ID
$TVARKIND  := `$ID | `$ID :: $KIND
$TVARKINDS := $TVARKIND | $TVARKIND , $TVARKINDS
$TVARS     := <$TVARKINDS>
```

### Qualifier

```
$QUALIFIER := shared | uniq
```

### Type

```
$QTYPE   := $QUALIFIER? $TYPE | $TVAR <$QTYPES>?
$TYPE    := $DOTID <$QTYPES>? | func ( $QTYPES? ) $RETTYPE |
           ( $QTYPES? ) | [ $QTYPE ]
$RETTYPE := -> $QTYPE
$QTYPES  := $QTYPE | $QTYPE , $QTYPES
```

### Type Specifier

```
$TYPESPEC := : $QTYPE
```

### Struct

Product type.
```
$STRUCT   := struct $ID $TVARS? $PREDS? { $PROD }
$PROD     := $PRODTYPE | $PRODTYPE ; $PROD
$PRODTYPE := $ID $TYPESPEC
```

Algebraic data type.

```
$data     := data $ID $TVARS? $PREDS? { $SUM }
$SUM      := $SUMTYPE | $SUMTYPE $SEP $SUM
$SUMTYPE  := $ID $SUMTYPES?
$SUMTYPES := : $QTYPE | : ($QTYPES)
```

## Function Definition

```
$DEFUN  := func $ID ( $ARGS? ) $RETTYPE? $PREDS? { $EXPRS }
$DEFUNS := $DEFUN | $DEFUNS $SEP $DEFUN
$ARGS   := $ARG | $ARG , $ARGS
$ARG    := $PATTERN $TYPESPEC?
```

```
func myfun (x : `a, y : `b) -> `a require num<`a>, bool<`b> { x }
```

## Expression

```
$EXPR    := $PREFIX? $EXPR | $EXPR $INFIX $EXPR | $EXPR0
$EXPR0   := $EXPR1 $EXPR2
$EXPR1   := $DOTID | $IF | $LET | ( $EXPR ) | $TUPLE |
            { $EXPRS } | [ $EXPRS'? ] | $LITERAL
$EXPR2   := ∅ | [ $EXPR ] $EXPR2 | $APPLY $EXPR2
$EXPRS   := $EXPR | $EXPR $SEP $EXPR
$EXPRS'  := $EXPR | $EXPR , $EXPR
$LITERAL := $STR | $CHAR | $FLOAT | $NATURAL
$PREFIX  := - | *
```

## Expression (Old)

```
$EXPR := $EXPR0 $EXPR' | $EXPR0 $EXPR' $INFIX+ $EXPR
$EXPR0 := $PREFIX? $EXPR0'
$EXPR0' := $DOTID | $IF | $LET | ( $EXPR , ) | ( $EXPR ) | ( $EXPRS_? ) |
           { $DICT } | { $EXPRS } | [ $EXPRS_? ] | $LITERAL
$EXPR' := ∅ | [ $EXPR ] $EXPR' | $APPLY $EXPR'
$EXPRS := $EXPR | $EXPR $SEP $EXPR
$EXPRS_ := $EXPR | $EXPR , $EXPR
$LITERAL := $STR | $DECIMAL | $FLOAT
$PREFIX := - | *
```

### Apply

```
$APPLY := ( $EXPRS'? )
```

TODO: named arguments, optional

### If

```
$IF   := if $EXPR { $EXPRS } $ELSE?
$ELSE := elif $EXPR { $EXPRS } $ELSE | else { $EXPRS }
```

### Let

```
$LET   := let $BINDS
$BIND  := $DBIND $TYPESPEC? = $EXPR
$BINDS := $BIND | $BIND , $BINDS
```

### Tuple

```
$TUPLE := ( $EXPR, ) | ( $EXPRS'? )
```

## String Literal

```
$STR
```

same as Haskell

## Character Literal

```
$CHAR
```

## Natural Number Literal

```
$NATURAL
```

same as Haskell

## Floating Point Number Literal

```
$FLOAT
```

same as Haskell

## Module Loading

There are the global and the module local loading paths for looking up modules.
For each module, the directory path, in which the module exists,
is set as the local loading path.
Imported modules are, first of all, looked up in the module local loading path and then the global loading path.

Assume that the global loading path is "/home/usr/.lunar/include",
and there is a file, "/home/usr/.lunar/include/bar/buzz.lunar",
in the global loading path.
When compiling "/home/user/project/foo.lunar",
the "/home/user/project" directory is set as the module
local loading path.
If "bar.buzz" is imported in the "foo.lunar",
"/home/user/project/bar/buzz.lunar" is looked up first of all.
If there is no "buzz.lunar" on the module local loading path,
then "/home/usr/.lunar/include/bar/buzz.lunar" is looked up finally.

## Built-in Type

```
bool | u64 | s64 | u32 | s32 | u16 | s16 | u8 | s8 | fp64 | fp32 | void
```

## References and Data Creation

### Reference

Data on stack is created as follows.

```
func expr() {
    let v = u32(10)    // Ref<u32>
    let w = bool(true) // Ref<bool>
}
```

or

```
func expr() {
    let v = stack u32(10)    // Ref<u32>
    let w = stack bool(true) // Ref<bool>
}
```

```
struct foo {
    a : u32
    b : bool
}

func expr() {
    let v = foo(10, true) // Ref<10>
}
```

or

```
struct foo {
    a : u32
    b : bool
}

func expr() {
    let v = stack foo(10, true) // Ref<10>
}
```

If you want to specify types for struct or unin, use "stack" operator.

```
struct foo<`t> {
    a : `t
    b : bool
}

func expr() {
    let v = foo(20, false) // Ref<foo<s64>>
}
```

```
struct foo<`t> {
    a : `t
    b : bool
}

func expr() {
    let v = stack foo<u32>(20, false) // Ref<foo<u32>>
}
```

```
data foo {
    a
    b : bool
}

func expr() {
    let v = a       // a of Ref<foo>
    let w = b(true) // b of Ref<foo>
}
```

```
struct foo {
    a : u32
    b : bool
}

data bar {
    L1
    L2 : foo
}

func expr() {
    let v = L2(10, true) // L2 of Ref<bar>
}
```

```
data foo {
    a
    b : bool
}

data bar {
    c
    d : foo
}

func expr() {
    let v = d(b(false)) // Ref<bar>
}
```

```
struct foo {
    a : u32
}

struct bar {
    b : foo
    c : bool
}

func expr() {
    let v = bar((20), true)     // OK
    let w = bar(foo(20), false) // OK
    let z = foo(30)
    let x = bar(z, true) // OK
}
```

### Linear Type and Reference

Linear data type, which is stored on heap, is created as follows.

```
func expr() {
    let v = new u32(10) // Lin<u32>
}
```

```
struct foo {
    a : u32
}

struct bar {
    b : Lin<foo>
}

func expr() {
    let v = new bar(new foo(20))
    v.a
}
```

Linear type contained by struct or data cannot be moved.

```
struct foo {
    a : Lin<u32>
    b : Maybe<Lin<bool>>
}

func expr() {
    let v = new foo(new u32(20), Just(new bool(true)))
    f(v.a) // error, could not move
    v.a := new u32(40) // assign
    v.b := Nothing     // assign
}
```

### Shared Type and Reference

Shared data type, which is stored on heap, is created as follows.

```
func expr() {
    let v = shared u32(10) // Shared<u32>
}
```

## Destructuring-bind

```
$DBIND   := _ | $DBINDSP | $DOTID $DBINDSP?
$DBINDS  := $DBIND | $DBIND , $DBINDS
$DBINDSP := ( $DBINDS )
```

## Pattern Match

```
$MATCH     := match expr { $PATEXPR+ }
$PATEXPR   := $PATTERN in $EXPR
$PATTERN   := _ | $PATTERNP | $DOTID $PATTERNP? | $LITERAL
$PATTERNS  := $PATTERN | $PATTERN , $PATTERNS
$PATTERNP := ( $PATTERNS )
```
