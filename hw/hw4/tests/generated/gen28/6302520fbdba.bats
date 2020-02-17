load ../../harness

@test "6302520fbdba" {
  check 'x :=  -1   * -4  ' '⇒ skip, {x → 4}'
}
