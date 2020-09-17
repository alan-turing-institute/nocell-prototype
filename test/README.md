## Test examples

This directory contains the expected output from the sequence

|        |               |      |                       |       |                          |            |                 |
| :-:    | :-:           | :-:  | :-:                   | :-:   | :-:                      | :-:        | :-:             |
| nocell | `eval` <br> → | cell | `stack->sheet` <br> → | sheet | `ods` <br> →             | sxml       | `srl:sxml->xml` |
|        |               |      |                       |       | `sheet-write-ods` <br> → | xml (fods) |     ↵           |
