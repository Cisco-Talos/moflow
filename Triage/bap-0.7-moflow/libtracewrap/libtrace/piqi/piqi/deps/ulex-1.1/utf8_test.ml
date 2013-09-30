#load "utf8.cmo";;

let () =
  let b = Buffer.create 10 in
  for i = 0 to 0x10ffff do
    if (i >= 0xd800) && (i <= 0xdfff) then ()
    else (
      (try Utf8.store b i with Utf8.MalFormed ->
	Printf.eprintf "Conversion failure %x\n" i; exit 1);
      let s = Buffer.contents b in
      Buffer.clear b;
      let j = 
	try Utf8.next s 0 with Utf8.MalFormed ->
	  Printf.eprintf "Deconversion failure %x (%S)\n" i s; exit 1 in
      if (i != j) then 
	(Printf.eprintf "Conversion/deconversion error. %x->%x\n" i j; exit 1)
    )
  done
