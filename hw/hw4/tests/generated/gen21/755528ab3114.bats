load ../../harness

@test "755528ab3114" {
  check 'if (true   ∧     y    + y <3-    4) then   
y:= 2*v    else   
skip     ' '⇒ skip, {}'
}
