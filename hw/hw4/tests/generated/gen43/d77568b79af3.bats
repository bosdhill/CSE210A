load ../../harness

@test "d77568b79af3" {
  check 'ng     :=     -1    -     x  ;  
 
x :=y    +   -1 ' '⇒ skip; x := (y+-1), {ng → -1}
⇒ x := (y+-1), {ng → -1}
⇒ skip, {ng → -1, x → -1}'
}
