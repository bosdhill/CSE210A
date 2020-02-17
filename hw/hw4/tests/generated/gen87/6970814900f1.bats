load ../../harness

@test "6970814900f1" {
  check 'if (¬(x  < z   +    3))  then z   := -3  else 
   
x     := z    *    2' '⇒ x := (z*2), {}
⇒ skip, {x → 0}'
}
