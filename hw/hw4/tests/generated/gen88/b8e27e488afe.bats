load ../../harness

@test "b8e27e488afe" {
  check 'z:=   0  *    2;
  z :=    1  *     1 ' '⇒ skip; z := (1*1), {z → 0}
⇒ z := (1*1), {z → 0}
⇒ skip, {z → 1}'
}
