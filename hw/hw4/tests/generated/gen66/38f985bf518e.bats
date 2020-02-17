load ../../harness

@test "38f985bf518e" {
  check 'if (z    *     x<3 - 1∨    false)  then z     :=  1+  2    else 
x:=-3     -     Z   ' '⇒ z := (1+2), {}
⇒ skip, {z → 3}'
}
