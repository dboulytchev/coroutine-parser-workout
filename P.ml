open GT

@type tree = S of char | Seq of tree * tree | E | EOF with show

type stream = char list

let of_string s =
  let n = String.length s in
  let rec loop i = 
    if i = n then [] else s.[i] :: loop (i+1)
  in
  loop 0

module Direct =
  struct

    type 'a p = stream -> ('a * stream) list

    let eof = function [] -> [EOF, []] | _ -> []

    let empty s = [E, s]

    let sym x = function
    | y::tl when y = x -> [S x, tl]
    | _ -> []

    let seq a b s =
      let a' = a s in
      List.flatten (
        List.map 
            (fun (at, s) -> 
            let b' = b s in
            List.map (fun (bt, s) -> Seq (at, bt), s) b'
          ) 
          a'
     )

    let alt a b s = (a s) @ (b s)

    let refine s = 
      try List.find (function (_, []) -> true | _ -> false) s |> fun x -> Some (fst x)
      with Not_found -> None

    let (|>) = seq
    let (<>) = alt

    let a = sym 'a'
    let b = sym 'b'
    let c = sym 'c'
        
    let test01 = a |> b |> c |> eof
    let test02 = (a <> b <> c) |> eof
      
    let test03 = 
      let rec pali s = (
        (a |> pali |> a) <>
        (b |> pali |> b) <>
        (c |> pali |> c) <>
        empty    
       ) s
      in
      pali |> eof

    let test04 =
      let rec expr s = (
        (expr |> sym '+' |> expr) <>
        a
       ) s
      in
      expr |> eof

    let run test input =
      Printf.printf "%s\n" @@
      show(option) (show(tree)) @@ refine (test @@ of_string input)

    let _ = 
      Printf.printf "Running direct style:\n";
(*       run test01 "abc";
      run test01 "abcd";
      run test02 "a";
      run test02 "b";
      run test02 "c";
      run test03 "abcccabaabaaabbbcccaaabbccccccbbaaacccbbbaaabaabacccba";
      (* loops: run test04 "a";  *)
      (* loops: run test04 "a+a" *)
*)
  end

module CPS =
  struct

    (* Result of "one-step" parsing *)
    type 'a result = 
    (*          parsed residual    residual parsing
                value   stream        steps
                   |      |             |
                   v      v             v
    *)
    | Ok       of 'a * stream * (unit -> 'a result) 
    (*             residual parsing
                        steps
                          |
                          v
    *)
    | Continue of (unit -> 'a result)   
    | End 

    let rec maps f k () = 
      match k () with
      | Ok (t, s, k) -> Ok (f t, s, maps f k)
      | Continue k   -> Continue (maps f k)
      | End          -> End

    (* One-step parser *)
    type 'a p = stream -> 'a result

    (* Some tracing noodles *)
    let log = ref false
    let log_on  () = log := true
    let log_off () = log := false

    let ticker () =
      let i = ref 0 in
      (fun s -> 
         if !log then (
            Printf.printf "%s %d\n%!" s !i;
           incr i;
           ignore (read_line ())
        )
      )

    (* Empty residual parsing steps *)
    let stop () = End

    (* Some primitive parsers *)
    let eof = function [] -> Ok (EOF, [], stop) | _ -> End

    let empty s = Ok (E, s, stop)

    let sym x = function
    | y::tl when y = x -> Ok (S x, tl, stop)
    | _ -> End

    let seq a b s =
      let rec inner ka kb () =
        match kb () with
        | Ok (tb, sb, kb) -> Ok (tb, sb, inner kb ka)
        | Continue kb     -> Continue (inner kb ka)
        | End             -> Continue ka
      in
      Continue (
        let rec outer ka kb () =
           match ka () with
           | Ok (ta, sa, ka) -> Continue (outer ka (inner kb (maps (fun tb -> Seq (ta, tb)) (fun () -> b sa))))
           | Continue ka     -> Continue (inner (outer ka stop) kb)
           | End             -> Continue kb
        in
        outer (fun () -> a s) stop
      )
(*
    (* Sequential combinator seq : tree p -> tree p -> tree p *)
    let rec seq a b s = 
      let t = ticker () in
      let rec outer a kb () =
        match a () with
        | Ok (ta, sa, ka) -> 
            t ("seq: a=" ^ show(tree) ta);
            Continue (
              let rec inner b () =
                match b () with
                | Ok (tb, sb, kb) -> 
                    t ("seq: b=" ^ show(tree) tb);  (* <- tracing *)
                    Ok (Seq (ta, tb), sb, inner kb) (* essential part *)
                | Continue kb -> 
                    t "seq: b=continue"; (* <- tracing *)
                    Continue (outer ka (inner kb)) (* (inner kb) essential part  (* <- problem *)*)
                | End -> 
                    t "seq: b=end";     (* tracing *)
                    Continue (outer ka stop) (* essential part *)
              in
              inner (fun () -> b sa)
            )
        | Continue ka -> 
            t "seq: a=continue"; 
            Continue (
              let rec inner ka kb () =
                match kb () with
                | Ok (tb, sb, kb) -> Ok (tb, sb, outer ka stop)
                | Continue kb     -> Continue (outer ka kb)
                | End             -> Continue ka
              in 
              inner ka kb 
            )
        | End -> 
            Continue kb
(*
            t "seq: a=end"; 
            End
*)
      in
      Continue (outer (fun () -> a s) stop)
*)
    (* Parallel combinator alt : tree p -> tree p -> tree p *)
    let alt a b s = 
      let t = ticker () in
      let rec inner a b () =
        match a () with
        | Ok (ta, sa, ka) -> 
            t ("alt:" ^ show(tree) ta); 
            Ok (ta, sa, inner b ka)
        | Continue ka -> 
            t "alt:continue"; 
            Continue (inner b ka)
        | End -> 
            t "alt:end"; 
            b ()
      in
      Continue (inner (fun () -> a s) (fun () -> b s))

    (* Apply gets the first result *)
    let apply p s =
      (* let t = ticker () in *)
      let rec inner p =
        match p () with
        | Ok (tp, sp, kp) -> Some tp
        | Continue kp     -> inner kp
        | End             -> None
      in
      inner (fun () -> p s)

    let (|>) = seq
    let (<>) = alt

    let a = sym 'a'
    let b = sym 'b'
    let c = sym 'c'
        
    let test01 = a |> b |> c |> eof

    let test02 = (a <> b <> c) |> eof
      
    let test03 = 
      let rec pali s = (
        (a |> pali |> a) <>
        (b |> pali |> b) <>
        (c |> pali |> c) <>
        empty    
       ) s
      in
      pali |> eof

    let test04 =
      let rec expr s = (
        (expr |> sym '+' |> expr) <>
        a
       ) s
      in
      expr |> eof

    let test05 =
      let rec inner s = (
        (inner |> a) <> empty
      ) s
      in
      inner |> eof

    let test06 =
      let rec addi  s = (mulli   <> (addi  |> sym '+' |> mulli  )) s
      and     mulli s = (primary <> (mulli |> sym '*' |> primary)) s
      and     primary = a <> b <> c in
      addi |> eof

    let expr n = 
      let rec e s = (m <> (e |> (sym '+' <> sym '-') |> m )) s
      and     m s = (p <> (m |> (sym '*' <> sym '/') |> p)) s
      and     p s = (n <> (sym '(' |> e |> sym ')')) s in
      e |> eof

    let test07 = expr (a <> b <> c)
    
    let test08 = expr (sym 'n')

    let integer = 
      let nzdigit   = List.fold_left (fun acc x -> acc <> sym x) (sym '1') (of_string "23456789") in
      let digit     = sym '0' <> nzdigit in 
      let rec num s = ((digit |> num) <> empty) s in
      (nzdigit |> num) <> (sym '0')
    
    let test09 = expr integer

    let run test input =
      Printf.printf "%s\n%!" @@
      show(option) (show(tree)) (apply test @@ of_string input)

    let _ = 
      Printf.printf "Running CPS-style:\n%!";

      run test01 "abc";
      run test01 "abcd";
      run test02 "a";
      run test02 "b";
      run test02 "c";

(*
      run test03 "abcccabaabaaabbbccccccbbbaaabaabacccba";       
      run test03 "abccbbaccbacaabbbbaabbbbaacabccabbccba";
      run test03 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
*)
      run test04 "a";
      run test04 "a+a";
      run test05 "aaaaa";
      run test06 "a";
      run test06 "b";

      run test06 "c";
      run test06 "a+a+a";        
      run test06 "a*b+c";
      
      run test07 "a*(b+c)";
      
      run test08 "n*(n+n)";
      run test08 "n/(n*(n+n)-n+n*n/(n-n*n+n))";

      (* run test08 "(n+(n+(n+(n+(n+(n+n))))))"; *)
      
      run test09 "1-2+0";
  end
  
