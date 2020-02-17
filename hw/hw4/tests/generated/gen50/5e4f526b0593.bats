load ../../harness

@test "5e4f526b0593" {
  check 'skip    ; z:=x -     -3     ' '⇒ z := (x--3), {}
⇒ skip, {z → 3}'
}
