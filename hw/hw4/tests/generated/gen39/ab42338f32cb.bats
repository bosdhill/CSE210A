load ../../harness

@test "ab42338f32cb" {
  check 'if (false∨     1<   -2    -  -1)    then  
y     := z      else   
 x   :=  4' '⇒ x := 4, {}
⇒ skip, {x → 4}'
}
