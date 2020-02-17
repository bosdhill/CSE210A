load ../../harness

@test "8e664f972295" {
  check 'if (¬false) then skip   else 
   x     :=  -2   -  tm' '⇒ skip, {}'
}
