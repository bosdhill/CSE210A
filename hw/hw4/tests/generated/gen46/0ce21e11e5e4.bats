load ../../harness

@test "0ce21e11e5e4" {
  check 'skip    ; jf:=  x    -     y   ' '⇒ jf := (x-y), {}
⇒ skip, {jf → 0}'
}
