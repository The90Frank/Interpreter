# Interpreter

A functional language interpreter written in OCaml, supporting dictionaries, binary trees, closures, and conditional expressions.

## Overview

The project contains two versions of the interpreter, developed across different academic terms:

- **InterpreteEstivo1617** - Dictionary-based interpreter with support for key-value collections, function application over dictionaries, and dictionary comparison
- **InterpreteInverno1718** - Extended version adding binary tree support with operations like `ApplyOver`, `Update`, and `Select`

## Supported language features

- Integer, boolean, and string values
- Variable binding (`Let` expressions)
- Function abstraction and application (closures with lexical scoping)
- Conditional expressions (`If-Then-Else`)
- Boolean logic (`AND`, `OR`, `NOT`)
- Integer arithmetic and comparison (`+`, `-`, `*`, `=`, `<`, `<=`)
- Dictionary creation, element access, comparison, and functional mapping
- Binary tree creation, traversal, update by path, and filtering by predicate (Winter version)

## Project structure

```
Interpreter/
├── InterpreteEstivo1617.ml   # Dictionary interpreter (Summer 2016/17)
├── InterpreteInverno1718.ml  # Tree interpreter (Winter 2017/18)
├── testEstivo1617.ml         # Test suite for dictionary version
└── testInverno1718.ml        # Test suite for tree version
```

## Usage

Load the interpreter and its tests in an OCaml toplevel:

```
#use "InterpreteEstivo1617.ml";;
#use "testEstivo1617.ml";;
```

or for the tree version:

```
#use "InterpreteInverno1718.ml";;
#use "testInverno1718.ml";;
```

## License

Apache License 2.0 - see [LICENSE](LICENSE) for details.
