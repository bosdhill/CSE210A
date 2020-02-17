load ../../harness

@test "7a179c072a56" {
  check 'x    :=  z   -  x    ; skip' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
