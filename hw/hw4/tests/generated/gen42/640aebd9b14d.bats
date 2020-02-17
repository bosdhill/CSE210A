load ../../harness

@test "640aebd9b14d" {
  check 'if (y  =0 ∧false)      then 

skip     else   iR     := y    *    0   ' '⇒ iR := (y*0), {}
⇒ skip, {iR → 0}'
}
