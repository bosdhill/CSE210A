load ../../harness

@test "26e18c5f7dd8" {
  check 'if (¬(1 + z    <     4  + -4)) then 
    z  :=     z      else   y   :=     4    - -3 ' '⇒ z := z, {}
⇒ skip, {z → 0}'
}
