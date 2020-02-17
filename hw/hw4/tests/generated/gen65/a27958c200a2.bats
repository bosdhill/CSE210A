load ../../harness

@test "a27958c200a2" {
  check 'if (false  ∨  -2    *    0     <    3 *-4)    then 
 skip    else 
 y    :=bs  +     0    ' '⇒ y := (bs+0), {}
⇒ skip, {y → 0}'
}
