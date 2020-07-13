module Js' = Js;
open! Promise;
module Js = Js';

module Impl = (Editor: Sig.Editor) => {
  module State = State.Impl(Editor);
  module ViewM = State__View.Impl(Editor);
  open Error;

  let handle =
    fun
    | Connection(error) => {
        let (header, body) = Connection.Error.toString(error);
        ViewM.displayError("Connection Error: " ++ header, Some(body));
      }
    | Parser(error) => {
        let body = Parser.Error.toString(error);
        ViewM.displayError("Internal Parse Error", Some(body));
      }
    | Cancelled => ViewM.displayError("Query Cancelled", None)
    | OutOfGoal =>
      ViewM.displayError(
        "Out of goal",
        Some("Please place the cursor in a goal"),
      );
};
