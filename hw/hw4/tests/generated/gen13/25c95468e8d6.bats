load ../../harness

@test "25c95468e8d6" {
  check 'while (¬(0    *   0<   2)) do skip' '⇒ skip, {}'
}
