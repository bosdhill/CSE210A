load ../../harness

@test "dc18a5dc9395" {
  check 'if (true     ∨true)    then   

 y    := 0    *    0     else skip    ' '⇒ y := (0*0), {}
⇒ skip, {y → 0}'
}
