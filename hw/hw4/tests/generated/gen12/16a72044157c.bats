load ../../harness

@test "16a72044157c" {
  check 'if (w    -  y    =    -2)  then 
  skip   else x   :=3 ' '⇒ x := 3, {}
⇒ skip, {x → 3}'
}
