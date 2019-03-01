#lang racket

(require rackunit
         math/array
         "../../grid.rkt"
         "../../cell.rkt"
         (submod "../../examples/discounted-cash-flow/dcf-nocell.rkt" example))

(let ((epsilon  1e-7)
      (actual   (sheet-eval (cell->grid result)))
      (expected (mutable-array #[#[2010 2011 2012 2013]
                                 #[-100 -50 150 500]
                                 #[0.03 'nothing 'nothing 'nothing] 
                                 #[0.1 'nothing 'nothing 'nothing] 
                                 #[1 'nothing 'nothing 'nothing] 
                                 #[1.1 'nothing 'nothing 'nothing] 
                                 #[1 0 0 0] 
                                 #[2010 0 0 0] 
                                 #[2010 'nothing 'nothing 'nothing] 
                                 #[0 1 2 3] 
                                 #[1 1.1 1.2100000000000002 1.3310000000000004] 
                                 #[-100 -45.45454545454545 123.96694214876031 375.65740045078877] 
                                 #[354.16979714500366 'nothing 'nothing 'nothing] 
                                 #[0 0 0 1] 
                                 #[0 0 0 1] 
                                 #[0 0 0 500] 
                                 #[500 'nothing 'nothing 'nothing] 
                                 #[1 'nothing 'nothing 'nothing] 
                                 #[1.03 'nothing 'nothing 'nothing] 
                                 #[515.0 'nothing 'nothing 'nothing] 
                                 #[0.07 'nothing 'nothing 'nothing] 
                                 #[7357.142857142857 'nothing 'nothing 'nothing] 
                                 #[1 'nothing 'nothing 'nothing] 
                                 #[1.1 'nothing 'nothing 'nothing] 
                                 #[1 0 0 0] 
                                 #[2010 0 0 0] 
                                 #[2010 'nothing 'nothing 'nothing] 
                                 #[0 1 2 3] 
                                 #[1 1.1 1.2100000000000002 1.3310000000000004] 
                                 #[7357.142857142857 6688.311688311687 6080.2833530106245 5527.530320918749] 
                                 #[0 0 0 5527.530320918749] 
                                 #[5527.530320918749 'nothing 'nothing 'nothing] 
                                 #[5881.700118063753 'nothing 'nothing 'nothing] 
                                 #[5881.700118063753 'nothing 'nothing 'nothing]])))
  (check-within actual expected epsilon))
