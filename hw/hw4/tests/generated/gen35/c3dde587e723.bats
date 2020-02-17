load ../../harness

@test "c3dde587e723" {
  check 'z:= z     +     4     ' '⇒ skip, {z → 4}'
}
