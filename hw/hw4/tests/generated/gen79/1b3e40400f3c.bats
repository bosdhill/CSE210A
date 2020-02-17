load ../../harness

@test "1b3e40400f3c" {
  check 'y :=-4*lj;  z    :=  x    * 0   ' '⇒ skip; z := (x*0), {y → 0}
⇒ z := (x*0), {y → 0}
⇒ skip, {y → 0, z → 0}'
}
