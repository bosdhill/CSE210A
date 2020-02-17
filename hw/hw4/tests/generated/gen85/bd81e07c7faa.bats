load ../../harness

@test "bd81e07c7faa" {
  check 'if (true    ∨     true)     then  
 skip  else skip   ' '⇒ skip, {}'
}
