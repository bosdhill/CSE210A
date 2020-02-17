load ../../harness

@test "1d3a21f9cd09" {
  check 'z :=     z *   75973735474556409745745717351296972106     ; 
   z := z    -   y  ' '⇒ skip; z := (z-y), {z → 0}
⇒ z := (z-y), {z → 0}
⇒ skip, {z → 0}'
}
