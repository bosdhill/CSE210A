load ../../harness

@test "8f19aedf1280" {
  check 'n     :=  x    * 0  ' '⇒ skip, {n → 0}'
}
