load ../../harness

@test "6b0c45442db5" {
  check 'if (¬(x     - 0=     -4     +    Xd)) then y  := -3    +  z   else 
 skip    ' '⇒ y := (-3+z), {}
⇒ skip, {y → -3}'
}
