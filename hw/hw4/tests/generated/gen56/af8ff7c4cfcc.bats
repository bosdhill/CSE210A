load ../../harness

@test "af8ff7c4cfcc" {
  check 'if (x    -  3=  z -3 ∧ 4   +    y <  1    *  Go)     then 
 skip      else 
 E     := y     -     z    ' '⇒ E := (y-z), {}
⇒ skip, {E → 0}'
}
