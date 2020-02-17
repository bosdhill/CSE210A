load ../../harness

@test "06a558bf8fa7" {
  check 'y:=   x*    -1     ;     z   :=x *(3  -     ii)  ' '⇒ skip; z := (x*(3-ii)), {y → 0}
⇒ z := (x*(3-ii)), {y → 0}
⇒ skip, {y → 0, z → 0}'
}
