load ../../harness

@test "d399dffe44dd" {
  check 'y:=    z-    4 ;

x:=  x   ' '⇒ skip; x := x, {y → -4}
⇒ x := x, {y → -4}
⇒ skip, {x → 0, y → -4}'
}
