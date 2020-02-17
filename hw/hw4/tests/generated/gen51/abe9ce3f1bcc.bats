load ../../harness

@test "abe9ce3f1bcc" {
  check 'x   :=     -3 +    z ;
 
 x     :=   3    +  z     ' '⇒ skip; x := (3+z), {x → -3}
⇒ x := (3+z), {x → -3}
⇒ skip, {x → 3}'
}
