load ../../harness

@test "aea30d4f3d75" {
  check 'if (true   ∧   c +-2    =2 +     1)     then Za  :=    x -     0 else  Em     :=  y*  z' '⇒ Em := (y*z), {}
⇒ skip, {Em → 0}'
}
