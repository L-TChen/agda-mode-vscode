open Belt;
open BsMocha.Mocha;
open BsMocha;
open Js.Promise;

module Impl = (Editor: Sig.Editor) => {
  module Test__Parser__SExpression = Test__Parser__SExpression.Impl(Editor);
  open! Test__Parser__SExpression;
  open Test__Util;

  // [SExpression] -> [Response.Prioritized.t]
  let toPrioritizedResponses = exprs => {
    // keeping the successful parsing result
    // Assert.fail on the failed ones
    exprs
    ->Array.map(Response.Prioritized.parse)
    ->Array.map(
        fun
        | Error(e) => {
            Assert.fail(Parser.Error.toString(e));
            [||];
          }
        | Ok(v) => [|v|],
      )
    ->Array.concatMany;
  };

  describe("when parsing responses", () =>
    Golden.getGoldenFilepathsSync("../../../../test/tests/Parser/Response")
    ->Array.forEach(filepath =>
        BsMocha.Promise.it("should golden test " ++ filepath, () =>
          Golden.readFile(filepath)
          |> then_(raw =>
               raw
               ->Golden.map(parseSExpression([||]))
               ->Golden.map(toPrioritizedResponses)
               ->Golden.map(
                   Strings.serializeWith(Response.Prioritized.toString),
                 )
               ->Golden.compare
             )
        )
      )
  );
};

include Impl(SigImpl);
