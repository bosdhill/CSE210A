load ../../harness

@test "169de83694f4" {
  check 'skip   ;  x:=   y   -    y     ' '⇒ x := (y-y), {}
⇒ skip, {x → 0}'
}
