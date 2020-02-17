load ../../harness

@test "22956aca6d4d" {
  check 'z:=   v *  d    ' '⇒ skip, {z → 0}'
}
