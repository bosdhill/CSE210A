load ../../harness

@test "805d6abc6416" {
  check 'if (true     ∨  true)      then   skip    else   skip' '⇒ skip, {}'
}
