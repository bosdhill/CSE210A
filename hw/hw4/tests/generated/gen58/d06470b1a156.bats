load ../../harness

@test "d06470b1a156" {
  check 'if (-4     <    j6 -2     ∧true) then z   := z    else   
skip   ' '⇒ z := z, {}
⇒ skip, {z → 0}'
}
