# The Specification of Lunar Language

```
$TOP  := $TOP2 $TOP | ∅
$TOP2 := $CLASSDECL | $INST | $DEFUN | $IMPORT
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
             match | module | import | return | as | here |
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
$CLASSDECL := class $ID < $TVARKIND > $PREDS? { $INTERFACES $WHITESPACE3* }
$INTERFACES := $INTERFACE | $INTERFACE $SEP $INTERFACES
$INTERFACE := $INTNAMES :: func ( $TYPES? ) $TYPESPEC
$INTNAMES := $INTNAME | $INTNAME , $INTNAMES
$INTNAME := $ID | infix $INFIX
```

Example:
```
class ord<`a> require eq<`a> {
    funcA, funcB :: func(`a) : `a
    infix <, infix > :: func(`a, `a) : bool
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
$PREDS := require $PREDS_
$PREDS_ := $PRED | $PRED, $PRED
$PRED := $DOTID <$TYPE>
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
$ARRNUM := * $DECIMAL | * $DECIMAL $ARRNUM
$IDTVAR := $DOTID | $TVAR
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
$PRODTYPE := $ID $TYPESPEC
```

```
$UNION := struct $ID $TVARS? $PREDS? { $SUM }
$SUM := $SUMTYPE | $SUMTYPE $SEP $SUM
$SUMTYPE := $ID | $ID $TYPESPEC
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
$EXPR0 := $PREFIX? $EXPR0'
$EXPR0' := $ID | $IF | $LET | ( $EXPR , ) | ( $EXPR ) | ( $EXPRS_? ) |
           { $DICT } | { $EXPRS } | [ $EXPRS_? ] | $LITERAL | $MALLOC
$EXPR' := ∅ | [ $EXPR ] $EXPR' | $APPLY $EXPR'
$EXPRS := $EXPR | $EXPR $SEP $EXPR
$EXPRS_ := $EXPR | $EXPR , $EXPR
$LITERAL := $STR | $DECIMAL | $FLOAT
$PREFIX := - | *
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

### New

```
$MALLOC = $NEW | $SHARED
$NEW = new $DOTID $APPLY
$SHARED = shared $DOTID $APPLY
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

### Constrained Reference

Data on stack is created as follows.

```
func expr() {
    let v = u32(10)    // CRef<u32>
    let w = bool(true) // CRef<bool>
}
```

```
struct foo {
    a : u32
    b : bool
}

func expr() {
    let v = foo(10, true) // CRef<10>
}
```

```
struct foo<`t> {
    a : `t
    b : bool
}

func expr() {
    let v = foo(20, false) // CRef<foo<s64>>
}
```

```
union foo {
    a
    b : true
}

func expr() {
    let v = a       // a of CRef<foo>
    let w = b(true) // b of CRef<foo>
}
```

```
struct foo {
    a : u32
    b : bool
}

union bar {
    L1
    L2 : foo
}

func expr() {
    let v = L2(10, true) // L2 of CRef<bar>
}
```

```
union foo {
    a
    b : bool
}

union bar {
    c
    d : foo
}

func expr() {
    let v = d(b(false)) // CRef<bar>
}
```

### Linear Type and It's Reference

Linear data type, which is stored on heap, is created as follows.

```
func expr() {
    let v = new u32(10) // Lin<u32>
}
```

```
struct foo {
    a : LinRef<u32>  // Lin<u32> or null
}
```

```
struct foo {
    a : u32
}

struct bar {
    b : LinRef<foo>
}

func expr() {
    let v = new bar(foo())
    v.a
}
```

Linear type contained by struct or union cannot be moved.
```
struct foo {
    a : Lin<u32>
}

func expr() {
    let v = new foo(new u32(10))
    let w = v.a // error
}
```

### Shared Reference

Shared data type, which is stored on heap, is created as follows.

```
func expr() {
    let v = shared u32(10) // Shared<u32>
}
```

```
struct foo {
    a : SharedRef<u32> // Shared<u32> or Null
}
```

## Pattern Match

TODO:
```
struct foo {
    a : LinRef<u32> // Lin<u32> or Null
}

func expr() {
    let x = new foo(new u32(10)) // Lin<foo>
}
```