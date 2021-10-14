let invalid_arg fmt = Format.kasprintf invalid_arg fmt

let make_good_test s =
  ( Printf.sprintf "%S" s,
    `Quick,
    fun () ->
      match Emile.List.of_string s with
      | Error (`Invalid (x, r)) -> invalid_arg "Invalid email address: %s%s" x r
      | Ok _ -> () )

exception Expected_error of Emile.t list

let sp = Format.asprintf

let () =
  Printexc.register_printer (function
    | Expected_error t ->
        Some (Fmt.str "Expected error: %a" Fmt.(Dump.list Emile.pp) t)
    | _ -> None)

let make_bad_test s =
  Alcotest.test_case (sp "%S" s) `Slow @@ fun () ->
  match Emile.List.of_string_with_crlf s with
  | Ok [ `Mailbox { Emile.domain = `Addr (Emile.Ext ("IPv6", v)), _; _ } ] -> (
      try
        ignore (Ipaddr.of_string_exn v) ;
        Alcotest.fail "Expected error"
      with _ -> ())
  | Ok [ `Mailbox { Emile.domain = `Addr (Emile.Ext _), _; _ } ] -> ()
  | Ok _ -> Alcotest.fail "Expected error"
  | Error _ -> ()

let tests =
  [
    "Mary Smith <mary@example.net>";
    "Mary Smith <mary@x.test>, jdoe@example.org, Who? <one@y.test>";
    "A Group:Ed Jones <c@a.test>,joe@where.test,John <jdoe@one.test>;";
    "Undisclosed recipients:;";
    "\"Mary Smith: Personal Account\" <smith@home.example>";
    "John Doe <jdoe@machine.example>";
    {|Pete(A nice \) chap) <pete(his account)@silly.test(his host)>|};
    "A Group(Some people)    :Chris Jones <c@(Chris's \
     host.)public.example>,      joe@example.org,  John <jdoe@one.test> (my \
     dear friend); (the end of the group)";
    "(Empty list)(start)Hidden recipients  :(nobody(that I know))  ;";
    "Mary Smith <@node.test:mary@example.net>, , jdoe@test  . example";
    "Joe Q. Public <john.q.public@example.com>";
    "John Doe <jdoe@machine(comment).  example>";
    "Mary Smith    <mary@example.net>";
    "<boss@nil.test>, \"Giant; \\\"Big\\\" Box\" <sysservices@example.net>";
    "!#$%&`*+/=?^`{|}~@iana.org";
    "(\x07;)mary@example.net";
    "\"\\\x0a\"@x.test";
    {|"\a"@x.test|};
    "\"\x07\"@x.test";
    "\"\\\x07\"@x.test";
    "pete@[255.255.255.255]";
    "\"mary\"@example.net";
    "\"\\\"\"@example.net";
    "\"john\".\"public\"@example.com";
    "\"mary smith\"@home.example";
    "\"mary\".smith@home.example";
    "\"mary\\\000\"@home.example";
    " richard @home.example";
    "richar@ home .example";
    "mary . smith@y.test";
    " jdoe@example.net";
    "   jdoe@example.net";
    "(comment)smith@home.example";
    "(comment(comment))smith@home.example";
    "smith@(comment)home.example";
    "smith@(comment)[255.255.255.255]";
    "robert@xn--hxajbheg2az3al.xn--jxalpdlp";
    "xn--robert@x.test";
    "stephane+blog@bortzmeyer.org";
    "{tropdur}@example.org";
    "c&a@hotmail.com";
    "directeur@arts-premiers.museum";
    "\"Stephane[Bortzmeyer]\"@laposte.net";
    "first.last@iana.org";
    "1234567890123456789012345678901234567890123456789012345678901234@iana.org";
    "\"first\\\"last\"@iana.org";
    "\"first@last\"@iana.org";
    "\"first\\last\"@iana.org";
    "first.last@[12.34.56.78]";
    "first.last@[IPv6:::12.34.56.78]";
    "first.last@[IPv6:1111:2222:3333::4444:12.34.56.78]";
    "first.last@[IPv6:1111:2222:3333:4444:5555:6666:12.34.56.78]";
    "first.last@[IPv6:::1111:2222:3333:4444:5555:6666]";
    "first.last@[IPv6:1111:2222:3333::4444:5555:6666]";
    "first.last@[IPv6:1111:2222:3333:4444:5555:6666::]";
    "first.last@[IPv6:1111:2222:3333:4444:5555:6666:7777:8888]";
    "first.last@x23456789012345678901234567890123456789012345678901234567890123.iana.org";
    "first.last@3com.com";
    "first.last@123.iana.org";
    "first.last@[IPv6:1111:2222:3333::4444:5555:12.34.56.78]";
    "first.last@[IPv6:1111:2222:3333::4444:5555:6666:7777]";
    "first.last@example.123";
    "first.last@com";
    "!#$%&'*+-/=?^_`.{|}~@example.com";
    "\"Abc@def\"@example.com";
    "\"Abc\\@def\"@iana.org";
    "\"Fred\\ Bloggs\"@iana.org";
    "\"Joe.\\Blow\"@iana.org";
    "\"Abc@def\"@iana.org";
    "\"Fred Bloggs\"@iana.org";
    "user+mailbox@iana.org";
    "customer/department=shipping@iana.org";
    "$A12345@iana.org";
    "!def!xyz%abc@iana.org";
    "_somename@iana.org";
    "dclo@us.ibm.com";
    "peter.piper@iana.org";
    "\"Doug \\\"Ace\\\" L.\"@iana.org";
    "test@iana.org";
    "TEST@iana.org";
    "1234567890@iana.org";
    "test+test@iana.org";
    "test-test@iana.org";
    "t*est@iana.org";
    "+1~1+@iana.org";
    "{_test_}@iana.org";
    "\"[[ test  ]]\"@iana.org";
    "test.test@iana.org";
    "\"test.test\"@iana.org";
    "test.\"test\"@iana.org";
    "\"test@test\"@iana.org";
    "test@123.123.123.x123";
    "test@123.123.123.123";
    "test@[123.123.123.123]";
    "test@example.iana.org";
    "test@example.example.iana.org";
    "\"test\\test\"@iana.org";
    "test@example";
    "\"test\\blah\"@iana.org";
    "\"test\\\"blah\"@iana.org";
    "customer/department@iana.org";
    "_Yosemite.Sam@iana.org";
    "~@iana.org";
    "\"Austin@Powers\"@iana.org";
    "Ima.Fool@iana.org";
    "\"Ima.Fool\"@iana.org";
    "\"Ima Fool\"@iana.org";
    "\"first\".\"last\"@iana.org";
    "\"first\".middle.\"last\"@iana.org";
    "\"first\".last@iana.org";
    "first.\"last\"@iana.org";
    "\"first\".\"middle\".\"last\"@iana.org";
    "\"first.middle\".\"last\"@iana.org";
    "\"first.middle.last\"@iana.org";
    "\"first..last\"@iana.org";
    "\"first\\\\\\\"last\"@iana.org";
    "first.\"mid\\dle\".\"last\"@iana.org";
    "\"test blah\"@iana.org";
    "(foo)cal(bar)@(baz)iamcal.com(quux)";
    "cal@iamcal(woo).(yay)com";
    "cal(woo(yay)hoopla)@iamcal.com";
    "cal(foo\\@bar)@iamcal.com";
    "cal(foo\\)bar)@iamcal.com";
    "first().last@iana.org";
    "pete(his account)@silly.test(his host)";
    "c@(Chris's host.)public.example";
    "jdoe@machine(comment). example";
    "1234 @ local(blah) .machine .example";
    "first(abc.def).last@iana.org";
    "first(a\"bc.def).last@iana.org";
    "first.(\")middle.last(\")@iana.org";
    "first(abc\\(def)@iana.org";
    "first.last@x(1234567890123456789012345678901234567890123456789012345678901234567890).com";
    "a(a(b(c)d(e(f))g)h(i)j)@iana.org";
    "name.lastname@domain.com";
    "a@b";
    "a@bar.com";
    "aaa@[123.123.123.123]";
    "a@bar";
    "a-b@bar.com";
    "+@b.c";
    "+@b.com";
    "a@b.co-foo.uk";
    "\"hello my name is\"@stutter.com";
    "\"Romain \" <romain@github.com>";
    "\"Test \\\"Fail\\\" Ing\"@iana.org";
    "valid@about.museum";
    "shaitan@my-domain.thisisminekthx";
    "foobar@192.168.0.1";
    "\"Joe\\Blow\"@iana.org";
    "HM2Kinsists@(that comments are allowed)this.is.ok";
    "user%uucp!path@berkeley.edu";
    "first.last @iana.org";
    "cdburgess+!#$%&'*-/=?+_{}|~test@gmail.com";
    "first.last@[IPv6:a1:a2:a3:a4:b1:b2:b3::]";
    "first.last@[IPv6:::]";
    "first.last@[IPv6:::b4]";
    "first.last@[IPv6:::b3:b4]";
    "first.last@[IPv6:a1::b4]";
    "first.last@[IPv6:a1::]";
    "first.last@[IPv6:a1:a2::]";
    "first.last@[IPv6:0123:4567:89ab:cdef::]";
    "first.last@[IPv6:0123:4567:89ab:CDEF::]";
    "first.last@[IPv6:::a3:a4:b1:ffff:11.22.33.44]";
    "first.last@[IPv6:a1:a2:a3:a4::11.22.33.44]";
    "first.last@[IPv6:a1:a2:a3:a4:b1::11.22.33.44]";
    "first.last@[IPv6:a1::11.22.33.44]";
    "first.last@[IPv6:a1:a2::11.22.33.44]";
    "first.last@[IPv6:0123:4567:89ab:cdef::11.22.33.44]";
    "first.last@[IPv6:0123:4567:89ab:CDEF::11.22.33.44]";
    "first.last@[IPv6:a1::b2:11.22.33.44]";
    "first.last@[IPv6:::a2:a3:a4:b1:b2:b3:b4]";
    "first.last@[IPv6:::a2:a3:a4:b1:ffff:11.22.33.44]";
    "test@test.com";
    "test@xn--example.com";
    "test@example.com";
    "用户@例子.广告";
    "अजय@डाटा.भारत";
    "квіточка@пошта.укр";
    "θσερ@εχαμπλε.ψομ";
    "Dörte@Sörensen.example.com";
    "коля@пример.рф";
  ]

let bad_tests =
  [
    "";
    "mary";
    "@";
    "mary@";
    "@io";
    "@example.net";
    ".mary@example.net";
    "jdoe.@example.net";
    "pete..silly.test";
    "sm_i-th.com";
    {|mary\@jdoe@one.test|};
    "jdoe@.one.test";
    "jdon@one.test.";
    "boss@nil..test";
    "\"\"\"@example.net";
    "\"\\\"@example.net";
    "jdoe\"@machine.example";
    "\"jdoe@machine.example";
    "\"john\"public@example.com";
    "john\"public\"@example.com";
    "\"john\"\"public\"@example.com";
    "\"mary\000\"@home.example";
    "pete@a[255.255.255.255]";
    "((comment)smith@home.example";
    "smith(coment)doe@home.example";
    "robert@henry.com\r";
    "(smith@home.example";
    "robert@[1.2.3.4";
    "\"john\\\"@example.com";
    "(comment\\)smith@home.example";
    "smith@home.example(comment\\)";
    "smith@home.example(comment\\";
    "robert@[RFC5322-[domain-literal\\]";
    "robert@[RFC5322-[domain-literal]";
    "robert@[RFC5322-[domain-literal\\";
    "marx@capitalism.ru\x0d";
    "\x0dmarx@capitalism.ru";
    "\"\x0dmarx\"@capitalism.ru";
    "(\x0d)marx@capitalism.ru";
    "marx@capitalism.ru(\x0d)";
    "smith@communism.uk\x0a";
    "\x0asmith@communism.uk";
    "\"\x0asmith\"@communism.uk";
    "(\x0a)smith@communism.uk";
    "smith@communism.uk(\x0a)";
    "first.last@sub.do,com";
    "first\\@last@iana.org";
    "first.last";
    ".first.last@iana.org";
    "first.last.@iana.org";
    "first..last@iana.org";
    "\"first\"last\"@iana.org";
    "\"\"\"@iana.org";
    "\"\\\"@iana.org";
    "\"\"@iana.org";
    "first\\@last@iana.org";
    "first.last@";
    "first.last@[.12.34.56.78]";
    "first.last@[12.34.56.789]";
    "first.last@[::12.34.56.78]";
    "first.last@[IPv5:::12.34.56.78]";
    "first.last@[IPv6:1111:2222:3333:4444:5555:12.34.56.78]";
    "first.last@[IPv6:1111:2222:3333:4444:5555:6666:7777:12.34.56.78]";
    "first.last@[IPv6:1111:2222:3333:4444:5555:6666:7777]";
    "first.last@[IPv6:1111:2222:3333:4444:5555:6666:7777:8888:9999]";
    "first.last@[IPv6:1111:2222::3333::4444:5555:6666]";
    "first.last@[IPv6:1111:2222:333x::4444:5555]";
    "first.last@[IPv6:1111:2222:33333::4444:5555]";
    "abc\\@def@iana.org";
    "abc\\@iana.org";
    "@iana.org";
    "doug@";
    "\"qu@iana.org";
    "ote\"@iana.org";
    ".dot@iana.org";
    "dot.@iana.org";
    "two..dot@iana.org";
    "\"Doug \"Ace\" L.\"@iana.org";
    "Doug\\ \\\"Ace\\\"\\ L\\.@iana.org";
    "hello world@iana.org";
    "gatsby@f.sc.ot.t.f.i.tzg.era.l.d.";
    "test.iana.org";
    "test.@iana.org";
    "test..test@iana.org";
    ".test@iana.org";
    "test@test@iana.org";
    "test@@iana.org";
    "-- test --@iana.org";
    "[test]@iana.org";
    "\"test\"test\"@iana.org";
    {|()[]\;:,><@iana.org|};
    "test@.";
    "test@example.";
    "test@.org";
    "test@[123.123.123.123";
    "test@123.123.123.123]";
    "NotAnEmail";
    "NotAnEmail";
    "\"test\"blah\"@iana.org";
    ".wooly@iana.org";
    "wo..oly@iana.org";
    "pootietang.@iana.org";
    ".@iana.org";
    "Ima Fool@iana.org";
    "phil.h\\@\\@ck@haacked.com";
    "first\\last@iana.org";
    "Abc\\@def@iana.org";
    "Fred\\ Bloggs@iana.org";
    "Joe.\\Blow@iana.org";
    "first.last@[IPv6:1111:2222:3333:4444:5555:6666:12.34.567.89]";
    "{^c\\@**Dog^}@cartoon.com";
    "cal(foo(bar)@iamcal.com";
    "cal(foo\\)@iamcal.com";
    "cal(foo)bar)@iamcal.com";
    "first(middle)last@iana.org";
    "a(a(b(c)d(e(f))g)(h(i)j)@iana.org";
    ".@";
    "@bar.com";
    "@@bar.com";
    "aaa.com";
    "aaa@.com";
    "aaa@.com";
    "aaa@.123";
    "aaa@[123.123.123.123]a";
    "aaa@[123.123.123.333]";
    "a@bar.com.";
    "-@..com";
    "-@a..com";
    "test@...........com";
    "\"\000 \"@char.com";
    "\000@char.com";
    "first.last@[IPv6::]";
    "first.last@[IPv6::::]";
    "first.last@[IPv6::b4]";
    "first.last@[IPv6::::b4]";
    "first.last@[IPv6::b3:b4]";
    "first.last@[IPv6::::b3:b4]";
    "first.last@[IPv6:a1:::b4]";
    "first.last@[IPv6:a1:]";
    "first.last@[IPv6:a1:::]";
    "first.last@[IPv6:a1:a2:]";
    "first.last@[IPv6:a1:a2:::]";
    "first.last@[IPv6::11.22.33.44]";
    "first.last@[IPv6::::11.22.33.44]";
    "first.last@[IPv6:a1:11.22.33.44]";
    "first.last@[IPv6:a1:::11.22.33.44]";
    "first.last@[IPv6:a1:a2:::11.22.33.44]";
    "first.last@[IPv6:0123:4567:89ab:cdef::11.22.33.xx]";
    "first.last@[IPv6:0123:4567:89ab:CDEFF::11.22.33.44]";
    "first.last@[IPv6:a1::a4:b1::b4:11.22.33.44]";
    "first.last@[IPv6:a1::11.22.33]";
    "first.last@[IPv6:a1::11.22.33.44.55]";
    "first.last@[IPv6:a1::b211.22.33.44]";
    "first.last@[IPv6:a1::11.22.33]";
    "first.last@[IPv6:a1::11.22.33.44.55]";
    "first.last@[IPv6:a1::b211.22.33.44]";
    "first.last@[IPv6:a1::b2::11.22.33.44]";
    "first.last@[IPv6:a1::b3:]";
    "first.last@[IPv6::a2::b4]";
    "first.last@[IPv6:a1:a2:a3:a4:b1:b2:b3:]";
    "first.last@[IPv6::a2:a3:a4:b1:b2:b3:b4]";
    "first.last@[IPv6:a1:a2:a3:a4::b1:b2:b3:b4]";
    "=?us-ascii?Q?Chri's_Smith?= =?us-ascii?Q?Henry?= \
     <.@gmail.com,@hotmail.fr:henry.chris+porno@(Chris's host.)public.example> \
     (je suis un connard en puissance)";
    "jdoe@[RFC-5322-\\a-domain-literal]";
    "jdoe@[RFC-5322-\\t-domain-literal]";
    "jdoe@[RFC-5322-\\]-domain-literal]";
    "jdoe@[RFC-5322-domain-literal] (comment)";
  ]

let domain = Alcotest.testable Emile.pp_domain Emile.equal_domain

let local = Alcotest.testable Emile.pp_local Emile.equal_local

type test =
  | V : 'a Alcotest.testable * ('a -> 'a -> int) * 'a list * 'a list -> test

let tests_on_order =
  [
    V
      ( domain,
        Emile.compare_domain,
        [ `Domain [ "gmail"; "com" ]; `Domain [ "x25519"; "net" ] ],
        [ `Domain [ "gmail"; "com" ]; `Domain [ "x25519"; "net" ] ] );
    V
      ( domain,
        Emile.compare_domain,
        [ `Domain [ "x25519"; "net" ]; `Domain [ "gmail"; "com" ] ],
        [ `Domain [ "gmail"; "com" ]; `Domain [ "x25519"; "net" ] ] );
    V
      ( domain,
        Emile.compare_domain,
        [ `Domain [ "foo" ]; `Domain [ "foo"; "bar" ] ],
        [ `Domain [ "foo" ]; `Domain [ "foo"; "bar" ] ] );
    V
      ( domain,
        Emile.compare_domain,
        [ `Domain [ "foo"; "bar" ]; `Domain [ "foo" ] ],
        [ `Domain [ "foo" ]; `Domain [ "foo"; "bar" ] ] );
    V
      ( domain,
        Emile.compare_domain,
        [
          `Addr (Emile.IPv4 Ipaddr.V4.any);
          `Addr (Emile.IPv4 Ipaddr.V4.localhost);
        ],
        [
          `Addr (Emile.IPv4 Ipaddr.V4.any);
          `Addr (Emile.IPv4 Ipaddr.V4.localhost);
        ] );
    V
      ( domain,
        Emile.compare_domain,
        [ `Addr (Emile.IPv4 Ipaddr.V4.any); `Domain [ "foo" ] ],
        [ `Addr (Emile.IPv4 Ipaddr.V4.any); `Domain [ "foo" ] ] );
    V
      ( domain,
        Emile.compare_domain,
        [ `Domain [ "foo" ]; `Addr (Emile.IPv4 Ipaddr.V4.any) ],
        [ `Addr (Emile.IPv4 Ipaddr.V4.any); `Domain [ "foo" ] ] );
    V
      ( domain,
        Emile.compare_domain,
        [ `Literal "foo"; `Literal "bar" ],
        [ `Literal "bar"; `Literal "foo" ] );
    V
      ( domain,
        Emile.compare_domain,
        [ `Literal "foo"; `Domain [ "foo" ]; `Addr (Emile.IPv4 Ipaddr.V4.any) ],
        [ `Addr (Emile.IPv4 Ipaddr.V4.any); `Literal "foo"; `Domain [ "foo" ] ]
      );
    V
      ( domain,
        Emile.compare_domain,
        [ `Literal "foo"; `Domain [ "foo" ] ],
        [ `Literal "foo"; `Domain [ "foo" ] ] );
    V
      ( domain,
        Emile.compare_domain,
        [ `Domain [ "foo" ]; `Literal "foo" ],
        [ `Literal "foo"; `Domain [ "foo" ] ] );
    V
      ( domain,
        Emile.compare_domain,
        [ `Addr (Emile.IPv4 Ipaddr.V4.localhost); `Literal "foo" ],
        [ `Addr (Emile.IPv4 Ipaddr.V4.localhost); `Literal "foo" ] );
    V
      ( domain,
        Emile.compare_domain,
        [ `Literal "foo"; `Addr (Emile.IPv4 Ipaddr.V4.localhost) ],
        [ `Addr (Emile.IPv4 Ipaddr.V4.localhost); `Literal "foo" ] );
    V
      ( local,
        Emile.compare_local,
        [ [ `Atom "foo"; `String "bar" ]; [ `String "bar"; `Atom "foo" ] ],
        [ [ `String "bar"; `Atom "foo" ]; [ `Atom "foo"; `String "bar" ] ] );
    V
      ( local,
        Emile.compare_local,
        [ [ `Atom "bar"; `Atom "foo" ]; [ `String "foo"; `String "bar" ] ],
        [ [ `Atom "bar"; `Atom "foo" ]; [ `String "foo"; `String "bar" ] ] );
    V
      ( local,
        Emile.compare_local,
        [ [ `Atom "foo" ]; [ `Atom "foo"; `Atom "bar" ] ],
        [ [ `Atom "foo" ]; [ `Atom "foo"; `Atom "bar" ] ] );
    V
      ( local,
        Emile.compare_local,
        [ [ `Atom "foo"; `Atom "bar" ]; [ `Atom "foo" ] ],
        [ [ `Atom "foo" ]; [ `Atom "foo"; `Atom "bar" ] ] );
  ]

let test_on_order _ (V (w, cmp, i, o)) =
  let pp = Alcotest.pp w in
  Alcotest.test_case (Format.asprintf "%a" Fmt.(Dump.list pp) i) `Quick
  @@ fun () ->
  let v = List.sort cmp i in
  Alcotest.(check (list w)) "order" v o

let make_test_serialisation s =
  ( Printf.sprintf "%S" s,
    `Quick,
    fun () ->
      match Emile.set_of_string s with
      | Error (`Invalid (x, r)) -> invalid_arg "Invalid email address: %s%s" x r
      | Ok v -> (
          let s' = Emile.set_to_string v in
          match Emile.set_of_string s' with
          | Error (`Invalid (x, r)) ->
              invalid_arg "Invalid email address: %s%s" x r
          | Ok _ -> ()) )

(* What You Write Is What You See *)
let iso_tests =
  [
    "\"K.\" <k.j@a.net>";
    "\"K. \" <k.j@a.net>";
    "a@b.com";
    "A <a@b.net>";
    "<@a.net:x@b.net>";
    "A <@a.net:x@b.net>";
    "<@a.net,@b.net:x@c.net>";
  ]

let make_test_iso s =
  Alcotest.test_case s `Quick @@ fun () ->
  match Emile.of_string s with
  | Ok v ->
      let s' = Emile.to_string v in
      Alcotest.(check string) "iso" s s'
  | Error err -> Alcotest.failf "%a" Emile.pp_error err

let () =
  Alcotest.run "Address test"
    [
      ("good", List.map make_good_test tests);
      ("bad", List.map make_bad_test bad_tests);
      ("order", List.mapi test_on_order tests_on_order);
      ("str", List.map make_test_serialisation tests);
      ("pretty-printer", List.map make_test_iso iso_tests);
    ]
