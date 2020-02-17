load ../../harness

@test "dbe5e6c9a92b" {
  check 'while false   ∨ y     +1 <    0  +    -4 do skip     ' '⇒ skip, {}'
}
