load ../../harness

@test "8f89085bf07a" {
  check 'J   :=0   -  z  ;
z:=    z    -z' '⇒ skip; z := (z-z), {J → 0}
⇒ z := (z-z), {J → 0}
⇒ skip, {J → 0, z → 0}'
}
