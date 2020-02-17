load ../../harness

@test "b09323997b9c" {
  check 'if (2  -    -4   = Sl     *     2 ∨   true)    then   skip   else y   :=   -1     +   -4     ' '⇒ skip, {}'
}
