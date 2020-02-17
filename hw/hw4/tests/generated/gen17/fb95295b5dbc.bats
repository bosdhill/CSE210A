load ../../harness

@test "fb95295b5dbc" {
  check 'while -1-    y    = -2  +  x ∧ x   <z     * -1     do   
 y:=x * -1 ' '⇒ skip, {}'
}
