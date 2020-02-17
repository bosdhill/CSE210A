load ../../harness

@test "9a93445c1bc2" {
  check 'x   :=  x     *  x' '⇒ skip, {x → 0}'
}
