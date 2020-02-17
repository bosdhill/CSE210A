load ../../harness

@test "c428024af444" {
  check 'z  := z     -   3; 

z    :=     -4  --3    ' '⇒ skip; z := (-4--3), {z → -3}
⇒ z := (-4--3), {z → -3}
⇒ skip, {z → -1}'
}
