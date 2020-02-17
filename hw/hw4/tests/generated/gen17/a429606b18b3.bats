load ../../harness

@test "a429606b18b3" {
  check 'x   :=-4   -y ; 



skip' '⇒ skip; skip, {x → -4}
⇒ skip, {x → -4}'
}
