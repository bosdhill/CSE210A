load ../../harness

@test "72dc2a78d0ea" {
  check 'while false ∧   y  < x    *-3 do Y:=z   -   y     ' '⇒ skip, {}'
}
