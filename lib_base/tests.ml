let add_basetests () =
  Maps.(
    StringTester.add_tests ();
    IntStringTester.add_tests ();
    CharIntTester.add_tests ();
    NativeIntTester.add_tests ();
    UcharStringTester.add_tests ();
  ); (*
  Sets.(
    UcharTester.add_tests ();
    NativeintTester.add_tests ();
    StringTester.add_tests ();
    IntTester.add_tests ();
    CharTester.add_tests ();
    FloatTester.add_tests ();
  ); *)

