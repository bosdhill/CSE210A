load ../../harness

@test "f304d7506130" {
  check 'while false    ∧    2   - 2     <   H   +  4      do x :=-2-    -1     ' '⇒ skip, {}'
}
