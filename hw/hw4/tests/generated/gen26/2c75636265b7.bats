load ../../harness

@test "2c75636265b7" {
  check 'tW:=  x     *x;z  := 3*     QR   ' '⇒ skip; z := (3*QR), {tW → 0}
⇒ z := (3*QR), {tW → 0}
⇒ skip, {tW → 0, z → 0}'
}
