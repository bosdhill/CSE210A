load ../../harness

@test "92747a6effac" {
  check 'if true    then  
q :=    x     else skip' '⇒ q := x, {}
⇒ skip, {q → 0}'
}
