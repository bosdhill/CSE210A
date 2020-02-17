load ../../harness

@test "fb15dc2ab694" {
  check 'z     :=  -2    * x     ;

x    :=3     ' '⇒ skip; x := 3, {z → 0}
⇒ x := 3, {z → 0}
⇒ skip, {x → 3, z → 0}'
}
