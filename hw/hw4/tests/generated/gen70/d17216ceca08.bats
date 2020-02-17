load ../../harness

@test "d17216ceca08" {
  check 'x :=z   -     z     ' '⇒ skip, {x → 0}'
}
