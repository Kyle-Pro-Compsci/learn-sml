(* Complete the expression *)
fun isValidDate(d, m) = 
case m of
"January" => d <= 31 andalso d > 0;

isValidDate(20, "January");
