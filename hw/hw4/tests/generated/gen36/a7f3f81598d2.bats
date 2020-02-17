load ../../harness

@test "a7f3f81598d2" {
  check 'if (true     ∧    false)     then  
 y  := rj *4      else 
 y  :=  -3-     x' '⇒ y := (-3-x), {}
⇒ skip, {y → -3}'
}
