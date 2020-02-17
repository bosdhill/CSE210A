load ../../harness

@test "f8713cba1926" {
  check 'y   :=  ZR   *    v;JM:=x*y  ' '⇒ skip; JM := (x*y), {y → 0}
⇒ JM := (x*y), {y → 0}
⇒ skip, {JM → 0, y → 0}'
}
