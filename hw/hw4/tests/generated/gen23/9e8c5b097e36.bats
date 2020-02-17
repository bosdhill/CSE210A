load ../../harness

@test "9e8c5b097e36" {
  check 'if (x  +   -2   =2 *     z    -3)  then 
s  :=     R     *     -1   else   z:= z     +-1 ' '⇒ z := (z+-1), {}
⇒ skip, {z → -1}'
}
