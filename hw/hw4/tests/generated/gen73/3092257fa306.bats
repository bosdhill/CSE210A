load ../../harness

@test "3092257fa306" {
  check 'z := 1 ;
skip   ' '⇒ skip; skip, {z → 1}
⇒ skip, {z → 1}'
}
