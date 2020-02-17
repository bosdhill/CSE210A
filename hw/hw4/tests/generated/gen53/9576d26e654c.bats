load ../../harness

@test "9576d26e654c" {
  check 'skip ; BA   :=  -4   *    x  ' '⇒ BA := (-4*x), {}
⇒ skip, {BA → 0}'
}
