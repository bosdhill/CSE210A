load ../../harness

@test "8054d5b3c7ce" {
  check 'x   :=     x     - z ; 
x  :=     2   +T1    ' '⇒ skip; x := (2+T1), {x → 0}
⇒ x := (2+T1), {x → 0}
⇒ skip, {x → 2}'
}
