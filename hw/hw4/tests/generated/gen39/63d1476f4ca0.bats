load ../../harness

@test "63d1476f4ca0" {
  check 'x    := -2     ;


 skip' '⇒ skip; skip, {x → -2}
⇒ skip, {x → -2}'
}
