load ../../harness

@test "cf94056cf115" {
  check 'z:=  y  +  z  ' '⇒ skip, {z → 0}'
}
