let () =
  Stdlibtest.Tests.add_basetests ();
  Stdlibtest_406.Maps.(
    StringTester.add_tests ();
    IntStringTester.add_tests ();
    CharIntTester.add_tests ();
    NativeIntTester.add_tests ();
    UcharStringTester.add_tests ();
  )
