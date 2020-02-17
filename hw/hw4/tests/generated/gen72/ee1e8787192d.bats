load ../../harness

@test "ee1e8787192d" {
  check 'b   :=    -3  * 0 ' '⇒ skip, {b → 0}'
}
