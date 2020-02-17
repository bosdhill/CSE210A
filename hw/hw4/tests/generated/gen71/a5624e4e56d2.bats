load ../../harness

@test "a5624e4e56d2" {
  check 'oe    :=  -1; 
 
y:=    x ' '⇒ skip; y := x, {oe → -1}
⇒ y := x, {oe → -1}
⇒ skip, {oe → -1, y → 0}'
}
