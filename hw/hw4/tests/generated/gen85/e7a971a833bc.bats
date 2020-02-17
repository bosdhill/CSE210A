load ../../harness

@test "e7a971a833bc" {
  check 'if (¬(IR     *     y < 2 --2))  then  
skip    else 
  

y :=3    -    2' '⇒ y := (3-2), {}
⇒ skip, {y → 1}'
}
