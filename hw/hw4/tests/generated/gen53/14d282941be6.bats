load ../../harness

@test "14d282941be6" {
  check 'z:=  3     *  x;
  y :=   x  +     4   ' '⇒ skip; y := (x+4), {z → 0}
⇒ y := (x+4), {z → 0}
⇒ skip, {y → 4, z → 0}'
}
