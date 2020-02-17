load ../../harness

@test "bd5ea9decd3d" {
  check 'if (false∧     false)     then y:=  -2   -    -1      else 

x     :=    y+-2 ' '⇒ x := (y+-2), {}
⇒ skip, {x → -2}'
}
