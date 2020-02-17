load ../../harness

@test "e55936ca69cc" {
  check 'if (¬true)      then 
 
skip      else 
y    :=     -2 --4     ' '⇒ y := (-2--4), {}
⇒ skip, {y → 2}'
}
