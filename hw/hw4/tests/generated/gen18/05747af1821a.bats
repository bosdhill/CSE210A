load ../../harness

@test "05747af1821a" {
  check 'if (¬true)  then z :=    I9+   4   else 
y :=    -2*     -3  ' '⇒ y := (-2*-3), {}
⇒ skip, {y → 6}'
}
