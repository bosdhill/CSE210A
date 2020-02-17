load ../../harness

@test "99bd2e023110" {
  check 'x   :=   y   + -4     ; 
 

K     :=     3     *  y' '⇒ skip; K := (3*y), {x → -4}
⇒ K := (3*y), {x → -4}
⇒ skip, {K → 0, x → -4}'
}
