load ../../harness

@test "949ab7e7c949" {
  check 'if (false     ∨     true) then skip   else C6 :=   2     +y ' '⇒ skip, {}'
}
