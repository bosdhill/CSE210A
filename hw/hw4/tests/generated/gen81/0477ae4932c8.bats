load ../../harness

@test "0477ae4932c8" {
  check 'if (y  -     x    =  z    * -1    ∨  u    -     x    <    -3 *     xI)      then 
z    :=0    - x  else skip   ' '⇒ z := (0-x), {}
⇒ skip, {z → 0}'
}
