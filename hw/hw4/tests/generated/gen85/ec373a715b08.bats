load ../../harness

@test "ec373a715b08" {
  check 'z  :=  z +  z' '⇒ skip, {z → 0}'
}
