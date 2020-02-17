load ../../harness

@test "922b207fe0de" {
  check 'if (¬false)   then  y     :=  z    *    2 else 
y   :=z -  -1' '⇒ y := (z*2), {}
⇒ skip, {y → 0}'
}
