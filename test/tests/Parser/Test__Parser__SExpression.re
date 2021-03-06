open BsMocha.Mocha;
open BsMocha;

open Js.Promise;

module Impl = (Editor: Sig.Editor) => {
  module Test__Util = Test__Util.Impl(Editor);
  open Test__Util;

  open Belt;

  // [Int] -> String -> [SExpression]
  let parseSExpression = (breakpoints, input) => {
    open Parser.Incr.Event;

    let output = ref([||]);

    let parser =
      Parser.SExpression.makeIncr(
        fun
        | Yield(Error((errNo, raw))) =>
          Assert.fail(
            "Failed when parsing S-expression: "
            ++ Parser.Error.toString(SExpression(errNo, raw)),
          )
        | Yield(Ok(a)) => Js.Array.push(a, output^) |> ignore
        | Stop => (),
      );

    input
    ->Js.String.trim
    ->Strings.breakInput(breakpoints)
    ->Array.map(Parser.split)
    ->Array.concatMany
    ->Array.forEach(Parser.Incr.feed(parser));

    output^;
  };

  describe("when parsing S-expressions as a whole", () =>
    Golden.getGoldenFilepathsSync("../../../../test/tests/Parser/SExpression")
    ->Array.forEach(filepath =>
        BsMocha.Promise.it("should golden test " ++ filepath, () =>
          Golden.readFile(filepath)
          |> then_(raw =>
               raw
               ->Golden.map(parseSExpression([||]))
               ->Golden.map(
                   Strings.serializeWith(Parser.SExpression.toString),
                 )
               ->Golden.compare
             )
        )
      )
  );

  describe("when parsing S-expressions incrementally", () =>
    Golden.getGoldenFilepathsSync("../../../../test/tests/Parser/SExpression")
    ->Array.forEach(filepath =>
        BsMocha.Promise.it("should golden test " ++ filepath, () =>
          Golden.readFile(filepath)
          |> then_(raw =>
               raw
               ->Golden.map(
                   parseSExpression([|3, 23, 171, 217, 1234, 2342, 3453|]),
                 )
               ->Golden.map(
                   Strings.serializeWith(Parser.SExpression.toString),
                 )
               ->Golden.compare
             )
        )
      )
  );
};

include Impl(SigImpl);
