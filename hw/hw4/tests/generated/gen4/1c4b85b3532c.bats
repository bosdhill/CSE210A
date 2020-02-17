load ../../harness

@test "1c4b85b3532c" {
  check 'if (le     *     z   <     x-   p     ∧     3  -  2 < 4    * x)   then  y     :=     z    else 
  y     :=     z- x    ' '⇒ y := (z-x), {}
⇒ skip, {y → 0}'
}
