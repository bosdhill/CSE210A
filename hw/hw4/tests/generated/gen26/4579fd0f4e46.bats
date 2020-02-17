load ../../harness

@test "4579fd0f4e46" {
  check 'if (¬true)      then   

 z    := -1    +   2    else 
z  := 4   +   z' '⇒ z := (4+z), {}
⇒ skip, {z → 4}'
}
