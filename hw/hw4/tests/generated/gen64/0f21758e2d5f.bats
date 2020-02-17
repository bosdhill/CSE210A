load ../../harness

@test "0f21758e2d5f" {
  check 'y   :=    4*    y  ;
z   :=   z    ' '⇒ skip; z := z, {y → 0}
⇒ z := z, {y → 0}
⇒ skip, {y → 0, z → 0}'
}
