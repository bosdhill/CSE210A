load ../../harness

@test "681eeb743f8c" {
  check 'if (¬true) then  

skip   else z    :=-4  *     z   ' '⇒ z := (-4*z), {}
⇒ skip, {z → 0}'
}
