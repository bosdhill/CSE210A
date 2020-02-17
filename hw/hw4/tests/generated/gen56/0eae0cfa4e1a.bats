load ../../harness

@test "0eae0cfa4e1a" {
  check 'if (¬(-1 -z     < x   *    y))  then skip else 
 z    :=   x     +    0' '⇒ z := (x+0), {}
⇒ skip, {z → 0}'
}
