load ../../harness

@test "6608cabedea8" {
  check 'y:=  z +   0    ;
skip    ' '⇒ skip; skip, {y → 0}
⇒ skip, {y → 0}'
}
