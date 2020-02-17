load ../../harness

@test "391b64f6f1db" {
  check 'if (¬(y  -    y =V+    x))  then  skip     else     y     :=y     -  y ' '⇒ y := (y-y), {}
⇒ skip, {y → 0}'
}
