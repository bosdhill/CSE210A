load ../../harness

@test "ba6034ba7b8f" {
  check 'if false    then  

x:=y  else 
 z     :=   y* -2' '⇒ z := (y*-2), {}
⇒ skip, {z → 0}'
}
