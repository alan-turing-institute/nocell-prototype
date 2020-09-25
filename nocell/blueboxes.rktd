862
((3) 0 () 1 ((q lib "nocell/main.rkt")) () (h ! (equal) ((c form c (c (? . 0) q define)) q (1439 . 2)) ((c def c (c (? . 0) q sum)) q (1000 . 3)) ((c def c (c (? . 0) q len)) q (1122 . 3)) ((c def c (c (? . 0) q if)) q (1277 . 5)) ((c def c (c (? . 0) q product)) q (1059 . 3)) ((c def c (c (? . 0) q +)) q (30 . 4)) ((c def c (c (? . 0) q expt)) q (414 . 4)) ((c form c (c (? . 0) q @)) q (1409 . 2)) ((c def c (c (? . 0) q >=)) q (902 . 4)) ((c def c (c (? . 0) q >)) q (805 . 4)) ((c def c (c (? . 0) q -)) q (126 . 4)) ((c def c (c (? . 0) q /)) q (318 . 4)) ((c form c (c (? . 0) q stack-print)) q (0 . 2)) ((c def c (c (? . 0) q <)) q (610 . 4)) ((c def c (c (? . 0) q *)) q (222 . 4)) ((c def c (c (? . 0) q nth)) q (1188 . 4)) ((c def c (c (? . 0) q <=)) q (707 . 4)) ((c def c (c (? . 0) q halt)) q (1381 . 2)) ((c def c (c (? . 0) q =)) q (513 . 4))))
procedure
(stack-print expr)
procedure
(+ a b) -> (Array number?)
  a : (Array number?)
  b : (Array number?)
procedure
(- a b) -> (Array number?)
  a : (Array number?)
  b : (Array number?)
procedure
(* a b) -> (Array number?)
  a : (Array number?)
  b : (Array number?)
procedure
(/ a b) -> (Array number?)
  a : (Array number?)
  b : (Array number?)
procedure
(expt a b) -> (Array number?)
  a : (Array number?)
  b : (Array number?)
procedure
(= a b) -> (Array boolean?)
  a : (Array number?)
  b : (Array number?)
procedure
(< a b) -> (Array boolean?)
  a : (Array number?)
  b : (Array number?)
procedure
(<= a b) -> (Array boolean?)
  a : (Array number?)
  b : (Array number?)
procedure
(> a b) -> (Array boolean?)
  a : (Array number?)
  b : (Array number?)
procedure
(>= a b) -> (Array boolean?)
  a : (Array number?)
  b : (Array number?)
procedure
(sum a) -> number?
  a : (Array number?)
procedure
(product a) -> number?
  a : (Array number?)
procedure
(len a) -> (Array number?)
  a : (Array any/c)
procedure
(nth n a) -> number?
  n : (Array number?)
  a : (Array number?)
procedure
(if test then else) -> any/c
  test : boolean?
  then : any/c
  else : any/c
procedure
(halt) -> halt?
syntax
(@ expr string ...)
syntax
(define id expr)
