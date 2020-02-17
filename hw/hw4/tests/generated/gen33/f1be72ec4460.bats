load ../../harness

@test "f1be72ec4460" {
  check 'x     :=   O-    -3 ;
x  :=   0     -  x    ' '⇒ skip; x := (0-x), {x → 3}
⇒ x := (0-x), {x → 3}
⇒ skip, {x → -3}'
}
