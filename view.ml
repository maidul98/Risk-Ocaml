let max_name_length = Map.


                        let print_territory name num_troops =
                          let name_length = String.length name in 

                          "**********\n" ^
                          "*        *\n" ^
                          "*        *\n" ^
                          "*   " ^ name ^ "     *\n" ^
                          "*   "^ string_of_int num_troops ^"     *\n" ^
                          "*        *\n" ^
                          "*        *\n" ^
                          "**********\n"

                            ANSITerminal.(print_string [red]                  "\n\n
 +-------------+        +------------+
|             |        |            |
|             +--------+            |
|             |        |            |
|             |        |            |
|             |        |            |
|             |        |            |
+-------------+        +------------+

\n");;

