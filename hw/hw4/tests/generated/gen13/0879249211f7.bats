load ../../harness

@test "0879249211f7" {
  check 'if (false     ∧   2- 4 <     2+z)     then x   := G    *-2      else x:=z   ' '⇒ x := z, {}
⇒ skip, {x → 0}'
}
