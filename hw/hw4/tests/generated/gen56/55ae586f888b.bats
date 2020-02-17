load ../../harness

@test "55ae586f888b" {
  check 'y:=     3     *    Yg   ; 
z    := y    *   3   ' '⇒ skip; z := (y*3), {y → 0}
⇒ z := (y*3), {y → 0}
⇒ skip, {y → 0, z → 0}'
}
