load ../../harness

@test "56edeabf8170" {
  check 'x   :=    3+   x;  
skip    ' '⇒ skip; skip, {x → 3}
⇒ skip, {x → 3}'
}
