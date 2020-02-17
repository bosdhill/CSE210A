load ../../harness

@test "3174da73ea8d" {
  check 'if (true   ∨     true) then skip    else p4    :=    (z   +  1)--3     ' '⇒ skip, {}'
}
