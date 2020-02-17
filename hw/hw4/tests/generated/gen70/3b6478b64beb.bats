load ../../harness

@test "3b6478b64beb" {
  check 'if (¬(y+  z <  x +    x))      then  y   :=   2  -  z    else  skip' '⇒ y := (2-z), {}
⇒ skip, {y → 2}'
}
