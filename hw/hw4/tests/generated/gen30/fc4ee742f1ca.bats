load ../../harness

@test "fc4ee742f1ca" {
  check 'if (1 + 0   < z∧     -1 -     -2  <  y     -     2)  then    y     :=2    +   y  else     z   :=   z   +    0' '⇒ z := (z+0), {}
⇒ skip, {z → 0}'
}
