load ../../harness

@test "54a1bc4d3f5c" {
  check 'if (¬true)     then  skip      else z  :=   z  ' '⇒ z := z, {}
⇒ skip, {z → 0}'
}
