load ../../harness

@test "1c7f326d4f28" {
  check 'if (¬true)   then   skip   else y   := 4-  gg  ' '⇒ y := (4-gg), {}
⇒ skip, {y → 4}'
}
