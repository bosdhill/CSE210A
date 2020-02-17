load ../../harness

@test "4523d119a564" {
  check 'if (true ∨    false)   then y :=y* -1     else  
   skip   ' '⇒ y := (y*-1), {}
⇒ skip, {y → 0}'
}
