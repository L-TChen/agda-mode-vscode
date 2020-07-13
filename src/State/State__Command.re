open Command;
module Js' = Js;
open! Promise;
module Js = Js';

module Impl = (Editor: Sig.Editor) => {
  module State = State.Impl(Editor);
  module ConnM = State__Connection.Impl(Editor);
  module DecoM = State__Decoration.Impl(Editor);
  module EditM = State__Editor.Impl(Editor);
  module ErroM = State__Error.Impl(Editor);
  module ViewM = State__View.Impl(Editor);
  let rec handle = (command, state: State.t) => {
    let header = Command.toString(command);
    switch (command) {
    | Load =>
      EditM.saveEditor(state)
      ->flatMap(EditM.saveCursor)
      ->flatMap(DecoM.removeAll)
      ->flatMap(ConnM.sendRequest(handle, Load))
    | Quit =>
      DecoM.removeAll(state)->flatMap(State.destroy)->map(() => state)
    | Restart =>
      Js.log("restart");
      resolved(state);
    | Compile => ConnM.sendRequest(handle, Compile, state)
    | ToggleDisplayOfImplicitArguments =>
      ConnM.sendRequest(handle, ToggleDisplayOfImplicitArguments, state)
    | ShowConstraints => ConnM.sendRequest(handle, ShowConstraints, state)
    | SolveConstraints(normalization) =>
      EditM.localOrGlobal(
        goal =>
          ConnM.sendRequest(
            handle,
            SolveConstraints(normalization, goal),
            state,
          ),
        ConnM.sendRequest(
          handle,
          SolveConstraintsGlobal(normalization),
          state,
        ),
        state,
      )
    | ShowGoals => ConnM.sendRequest(handle, ShowGoals, state)
    | NextGoal => EditM.nextGoal(state)
    | PreviousGoal => EditM.previousGoal(state)
    | SearchAbout(normalization) =>
      ViewM.query(
        header,
        Some("name:"),
        None,
        expr =>
          ConnM.sendRequest(handle, SearchAbout(normalization, expr), state),
        state,
      )
    | Give =>
      EditM.localOrGlobal2(
        (goal, _) => ConnM.sendRequest(handle, Give(goal), state),
        goal =>
          ViewM.query(
            header,
            Some("expression to give:"),
            None,
            expr => {
              EditM.modifyGoal(goal, _ => expr, state)
              ->flatMap(ConnM.sendRequest(handle, Give(goal)))
            },
            state,
          ),
        ErroM.handle(OutOfGoal, state),
        state,
      )
    | Refine =>
      EditM.localOrGlobal(
        goal =>
          EditM.saveCursor(state)
          ->flatMap(ConnM.sendRequest(handle, Refine(goal))),
        ErroM.handle(OutOfGoal, state),
        state,
      )
    | _ => resolved(state)
    };
  };
};
