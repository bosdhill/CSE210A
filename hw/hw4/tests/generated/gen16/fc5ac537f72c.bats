load ../../harness

@test "fc5ac537f72c" {
  check 'if (I3*3    <  z -    z)   then skip    else z    :=   u  -     x    ' '⇒ z := (u-x), {}
⇒ skip, {z → 0}'
}
