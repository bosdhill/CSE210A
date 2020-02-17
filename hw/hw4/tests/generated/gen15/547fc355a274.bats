load ../../harness

@test "547fc355a274" {
  check 'if (¬true)   then  skip      else F:=z+   x   ' '⇒ F := (z+x), {}
⇒ skip, {F → 0}'
}
