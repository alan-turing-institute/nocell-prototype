(

 (define (total discounted-cashflows discounted-terminal-value)
   (+ (foldl (lambda (t c) (+ t c)) 0 discounted-cashflows) discounted-terminal-value))
 
 (define (dcf-value years cashflows terminal-growth discount-rate)

   (define (terminal-value)
     (/ (* (last cashflows) (+ 1 terminal-growth)) (- discount-rate terminal-growth)))

   (define (discount year value)
     (/ value (expt (+ 1 discount-rate) (- year (first years)))))

   (let ([discounted-cashflows
          (map (lambda (year cashflow) (discount year cashflow)) years cashflows)]
         [discounted-terminal-value
          (discount (last years) (terminal-value))])
     (total discounted-cashflows discounted-terminal-value)))

 (dcf-value (2020 2021 2022 2023) (-100 -50 150 500) 0.03 0.1))
