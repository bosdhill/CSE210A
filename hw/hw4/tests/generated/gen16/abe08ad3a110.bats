load ../../harness

@test "abe08ad3a110" {
  check 'if (true∨     z* z <    4 -  z)    then x   :=    f5     -q      else  y   :=2   -  1' '⇒ x := (f5-q), {}
⇒ skip, {x → 0}'
}
