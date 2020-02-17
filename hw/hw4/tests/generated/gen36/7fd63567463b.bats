load ../../harness

@test "7fd63567463b" {
  check 'if (¬(z    -  x=    2 +    z))    then 
y     := x+    z    else  skip     ' '⇒ y := (x+z), {}
⇒ skip, {y → 0}'
}
