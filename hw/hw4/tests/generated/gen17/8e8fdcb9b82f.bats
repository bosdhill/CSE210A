load ../../harness

@test "8e8fdcb9b82f" {
  check 'if (¬true)   then 
 
 
skip    else    y     := 3' '⇒ y := 3, {}
⇒ skip, {y → 3}'
}
