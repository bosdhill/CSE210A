load ../../harness

@test "e65d597393fb" {
  check 'if (1     +    z =  -3   -     4  ∧z  *   (z    -     x)    <     4+   -4)  then z    :=     2     +0      else    k   := x+     1     ' '⇒ k := (x+1), {}
⇒ skip, {k → 1}'
}
