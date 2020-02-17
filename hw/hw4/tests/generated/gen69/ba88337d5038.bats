load ../../harness

@test "ba88337d5038" {
  check 'x   :=   -1+  y;
 
 x   :=   OH-  x  +    0   ' '⇒ skip; x := ((OH-x)+0), {x → -1}
⇒ x := ((OH-x)+0), {x → -1}
⇒ skip, {x → 1}'
}
