load ../../harness

@test "1cbff7575108" {
  check 'if (¬false) then  y    :=-3-  -4   else M   :=  -1    +  3' '⇒ y := (-3--4), {}
⇒ skip, {y → 1}'
}
