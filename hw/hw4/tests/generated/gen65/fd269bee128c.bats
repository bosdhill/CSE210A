load ../../harness

@test "fd269bee128c" {
  check 'y   :=     x    + z   +   x     ;skip' '⇒ skip; skip, {y → 0}
⇒ skip, {y → 0}'
}
