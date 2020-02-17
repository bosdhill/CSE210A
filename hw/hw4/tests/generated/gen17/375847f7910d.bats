load ../../harness

@test "375847f7910d" {
  check 'y:=  z+     -2   ;
  
y :=    -2 ' '⇒ skip; y := -2, {y → -2}
⇒ y := -2, {y → -2}
⇒ skip, {y → -2}'
}
