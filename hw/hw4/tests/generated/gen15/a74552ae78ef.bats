load ../../harness

@test "a74552ae78ef" {
  check 'skip   ;y:=    -3     *    y  ' '⇒ y := (-3*y), {}
⇒ skip, {y → 0}'
}
