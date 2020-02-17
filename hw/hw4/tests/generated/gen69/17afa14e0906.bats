load ../../harness

@test "17afa14e0906" {
  check 'if (¬(y     *   0  <     z  -3))     then  



y   :=  y*    2    else skip' '⇒ y := (y*2), {}
⇒ skip, {y → 0}'
}
