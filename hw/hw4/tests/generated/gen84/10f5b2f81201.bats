load ../../harness

@test "10f5b2f81201" {
  check 'x:=z    -    -2   ;

z :=   y  *-3   ' '⇒ skip; z := (y*-3), {x → 2}
⇒ z := (y*-3), {x → 2}
⇒ skip, {x → 2, z → 0}'
}
