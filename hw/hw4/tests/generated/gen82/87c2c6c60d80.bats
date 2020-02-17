load ../../harness

@test "87c2c6c60d80" {
  check 'l    :=     z    * 1  ;
x  := y -     -1 ' '⇒ skip; x := (y--1), {l → 0}
⇒ x := (y--1), {l → 0}
⇒ skip, {l → 0, x → 1}'
}
