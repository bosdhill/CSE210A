load ../../harness

@test "f385cd83ac7c" {
  check 'y     :=0    +    -2 ;  x  :=     k    *    0   ' '⇒ skip; x := (k*0), {y → -2}
⇒ x := (k*0), {y → -2}
⇒ skip, {x → 0, y → -2}'
}
