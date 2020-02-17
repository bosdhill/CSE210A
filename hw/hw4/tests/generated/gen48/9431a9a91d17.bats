load ../../harness

@test "9431a9a91d17" {
  check 'if (true  ∧    true)    then  
 skip else      skip   ' '⇒ skip, {}'
}
