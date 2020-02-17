load ../../harness

@test "434888e9240a" {
  check 'if (¬true)    then 
skip      else 
skip    ' '⇒ skip, {}'
}
