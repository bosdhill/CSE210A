load ../../harness

@test "a2374e51cfbd" {
  check 'z:=x  --3;


y :=y   *  y   ' '⇒ skip; y := (y*y), {z → 3}
⇒ y := (y*y), {z → 3}
⇒ skip, {y → 0, z → 3}'
}
