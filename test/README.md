## Test examples

This directory contains the expected output from the sequence

|                   |               |                 |                       |       |                          |            |                 |
| :-:               | :-:           | :-:             | :-:                   | :-:   | :-:                      | :-:        | :-:             |
| stack-lang/nocell | `eval` <br> → | stack-lang/cell | `stack->sheet` <br> → | sheet | `ods` <br> →             | sxml       | `srl:sxml->xml` |
|                   |               |                 |                       |       | `sheet-write-ods` <br> → | xml (fods) |     ↵           |
