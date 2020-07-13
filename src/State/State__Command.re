open Command;
module Js' = Js;
open! Promise;
module Js = Js';

module Impl = (Editor: Sig.Editor) => {
  module State = State.Impl(Editor);
  module DecoM = State__Decoration.Impl(Editor);
  module ConnM = State__Connection.Impl(Editor);
  module EditM = State__Editor.Impl(Editor);
  let rec handle = (command, state: State.t) => {
    // let header = Command.toString(command);
    switch (command) {
    | Load =>
      Js.log("LOAD");
      EditM.saveEditor(state)
      ->flatMap(EditM.saveCursor)
      ->flatMap(DecoM.removeAll)
      ->flatMap(ConnM.sendRequest(handle, Load));
    | _ => resolved(state)
    };
  };
};
