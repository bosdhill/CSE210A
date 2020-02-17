load ../../harness

@test "52fc64f1fd83" {
  check 'if (¬true)      then 
skip    else 
skip   ' '⇒ skip, {}'
}
