load ../../harness

@test "4bf6b9445437" {
  check 'if (-3   --4    <   Ux  * 0   ∨     true) then skip      else  skip    ' '⇒ skip, {}'
}
