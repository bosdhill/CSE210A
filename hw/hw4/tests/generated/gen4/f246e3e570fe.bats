load ../../harness

@test "f246e3e570fe" {
  check 'if (¬(2 -    -2   *     3     <     0)) then 
   y :=  x  else 
 skip  ' '⇒ y := x, {}
⇒ skip, {y → 0}'
}
