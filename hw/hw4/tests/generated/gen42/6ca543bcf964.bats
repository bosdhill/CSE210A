load ../../harness

@test "6ca543bcf964" {
  check 'if true     then 
   skip  else skip    ' '⇒ skip, {}'
}
