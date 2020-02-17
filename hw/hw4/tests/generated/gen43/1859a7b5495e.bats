load ../../harness

@test "1859a7b5495e" {
  check 'if (false  ∧    false)      then  
y  :=   4  * 0 else  
 f:=   y   + z  ' '⇒ f := (y+z), {}
⇒ skip, {f → 0}'
}
