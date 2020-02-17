load ../../harness

@test "fa536f201383" {
  check 'if (4  *    -1     <    m * -1∧   true) then vf:=     1 +    z   else   skip ' '⇒ vf := (1+z), {}
⇒ skip, {vf → 1}'
}
