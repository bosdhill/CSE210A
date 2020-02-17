load ../../harness

@test "475828a86895" {
  check 'if (¬(0   * 1<   z    +y))  then  
y  :=     x-  qr  else   y     :=   -1 +    z' '⇒ y := (x-qr), {}
⇒ skip, {y → 0}'
}
