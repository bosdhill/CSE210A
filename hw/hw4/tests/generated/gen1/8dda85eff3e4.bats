load ../../harness

@test "8dda85eff3e4" {
  check 'if (¬true)  then 
   skip else 



skip' '⇒ skip, {}'
}
