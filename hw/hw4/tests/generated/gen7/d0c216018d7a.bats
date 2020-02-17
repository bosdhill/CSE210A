load ../../harness

@test "d0c216018d7a" {
  check 'if (z -  x   =    y    ∧   2     <  z+x)    then  z    :=     -4 +     z else     y:=  x-    z ' '⇒ y := (x-z), {}
⇒ skip, {y → 0}'
}
