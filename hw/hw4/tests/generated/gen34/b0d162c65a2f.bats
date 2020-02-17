load ../../harness

@test "b0d162c65a2f" {
  check 'z :=y +-4     ;
  y     :=  y +  0' '⇒ skip; y := (y+0), {z → -4}
⇒ y := (y+0), {z → -4}
⇒ skip, {y → 0, z → -4}'
}
