load ../../harness

@test "c02faf844d83" {
  check 'if (¬(xI=    1    + z))     then H  :=   y+   2      else 
skip' '⇒ H := (y+2), {}
⇒ skip, {H → 2}'
}
