load ../../harness

@test "b1badaeab136" {
  check 'y  :=  1  ;  
 z  :=     z    +    x     ' '⇒ skip; z := (z+x), {y → 1}
⇒ z := (z+x), {y → 1}
⇒ skip, {y → 1, z → 0}'
}
