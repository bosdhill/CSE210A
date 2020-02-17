load ../../harness

@test "c51c03ae037e" {
  check 'x     :=     -2     +   y    ;
 
f     :=  y    -     2  ' '⇒ skip; f := (y-2), {x → -2}
⇒ f := (y-2), {x → -2}
⇒ skip, {f → -2, x → -2}'
}
