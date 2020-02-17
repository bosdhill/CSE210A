load ../../harness

@test "4348622da752" {
  check 'if (¬(0  - y    <   -4+     2))    then  
x := 1  *y      else skip     ' '⇒ x := (1*y), {}
⇒ skip, {x → 0}'
}
