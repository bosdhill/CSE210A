load ../../harness

@test "bb9d9b252aac" {
  check 'if (z  - x  <y  +   2     ∧    false)      then 
 
skip    else     y :=    x -z    ' '⇒ y := (x-z), {}
⇒ skip, {y → 0}'
}
