load ../../harness

@test "83627b5df9ad" {
  check 'x    :=NZ    -  -2 ' '⇒ skip, {x → 2}'
}
