load ../../harness

@test "461a3f07567c" {
  check 'x   :=x    *-2    ' '⇒ skip, {x → 0}'
}
