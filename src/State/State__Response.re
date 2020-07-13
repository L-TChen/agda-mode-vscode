module Js' = Js;
open! Promise;
module Js = Js';

module Impl = (Editor: Sig.Editor) => {
  module State = State.Impl(Editor);
  module Request = Request.Impl(Editor);
  module ViewM = State__View.Impl(Editor);
  module DecoM = State__Decoration.Impl(Editor);
  module EditM = State__Editor.Impl(Editor);
  module ErroM = State__Error.Impl(Editor);
  open Response;
  open Belt;

  module DisplayInfo = {
    let handle =
      fun
      | Response.DisplayInfo.CompilationOk =>
        ViewM.displaySuccess("Compilation Done!", None)
      | Constraints(None) => ViewM.display("No Constraints", None)
      | Constraints(Some(payload)) =>
        ViewM.display("Constraints", Some(payload))
      | AllGoalsWarnings(header, "nil") => ViewM.displaySuccess(header, None)
      | AllGoalsWarnings(header, body) => ViewM.display(header, Some(body))
      | Time(payload) => ViewM.display("Time", Some(payload))
      | Error(payload) => ViewM.displayError("Error", Some(payload))
      | Intro(payload) => ViewM.display("Intro", Some(payload))
      | Auto(payload) => ViewM.displaySuccess("Auto", Some(payload))
      | ModuleContents(payload) =>
        ViewM.display("Module Contents", Some(payload))

      | SearchAbout(payload) => ViewM.display("Search About", Some(payload))

      | WhyInScope(payload) => ViewM.display("Scope info", Some(payload))
      | NormalForm(payload) => ViewM.display("Normal form", Some(payload))

      | GoalType(payload) => ViewM.display("Goal Type", Some(payload))
      | CurrentGoal(payload) => ViewM.display("Current goal", Some(payload))

      | InferredType(payload) =>
        ViewM.display("Inferred type", Some(payload))
      | Context(payload) => ViewM.display("Context", Some(payload))
      | HelperFunction(payload) => (
          state =>
            Editor.copyToClipboard(payload)
            ->flatMap(() =>
                ViewM.display(
                  "Helper function (copied to clipboard)",
                  Some(payload),
                  state,
                )
              )
        )
      | Version(payload) => ViewM.display("Version", Some(payload));
  };

  let handle = (dispatchCommand, sendRequest, response, state) => {
    switch (response) {
    | HighlightingInfoDirect(_remove, annotations) =>
      DecoM.add(annotations, state)
    | HighlightingInfoIndirect(filepath) =>
      let readFile = N.Util.promisify(N.Fs.readFile);
      readFile(. filepath)
      ->Promise.Js.fromBsPromise
      ->Promise.Js.toResult
      ->flatMap(
          fun
          | Ok(content) => {
              open! Parser.SExpression;
              let expressions =
                content->Node.Buffer.toString->Parser.SExpression.parse;

              // TODO: we should do something about these parse errors
              let _parseErrors: array((int, string)) =
                expressions->Array.keepMap(
                  fun
                  | Error(error) => Some(error)
                  | Ok(_) => None,
                );

              let annotations: array(Highlighting.t) =
                expressions
                ->Array.keepMap(
                    fun
                    | Error(_) => None // filter errors out
                    | Ok(L(xs)) =>
                      Some(Highlighting.parseIndirectHighlightings(xs))
                    | Ok(_) => Some([||]),
                  )
                ->Array.concatMany;

              DecoM.add(annotations, state);
            }
          // TODO: we should do something about these parse errors
          | Error(_err) => resolved(state),
        );
    | Status(_displayImplicit, _checked) => resolved(state)
    | JumpToError(filepath, offset) =>
      // only jump to site of error
      // when it's on the same file
      switch (Editor.getFileName(state.editor)) {
      | None => resolved(state)
      | Some(path) =>
        if (path == filepath) {
          EditM.setCursor(offset - 1, state);
        } else {
          resolved(state);
        }
      }
    | InteractionPoints(indices) =>
      EditM.instantiateGoals(indices, state)->flatMap(EditM.restoreCursor)
    | GiveAction(index, give) =>
      let found = state.goals->Array.keep(goal => goal.index == index);
      switch (found[0]) {
      | None =>
        ViewM.displayError(
          "Error: Give failed",
          Some("Cannot find goal #" ++ string_of_int(index)),
          state,
        )
      | Some(goal) =>
        (
          switch (give) {
          | Paren =>
            EditM.modifyGoal(goal, content => "(" ++ content ++ ")", state)
          | NoParen =>
            // do nothing
            resolved(state)
          | String(content) =>
            EditM.modifyGoal(
              goal,
              _ => Js.String.replaceByRe([%re "/\\\\n/g"], "\n", content),
              state,
            )
          }
        )
        ->flatMap(EditM.removeBoundaryAndDestroyGoal(goal))
      };
    | MakeCase(makeCaseType, lines) =>
      EditM.localOrGlobal(
        goal => {
          switch (makeCaseType) {
          | Function =>
            EditM.replaceWithLines(goal, lines, state)
            ->flatMap(dispatchCommand(Command.Load))
          | ExtendedLambda =>
            EditM.replaceWithLambda(goal, lines, state)
            ->flatMap(dispatchCommand(Command.Load))
          }
        },
        ErroM.handle(OutOfGoal, state),
        state,
      )
    | SolveAll(solutions) =>
      let solveOne = ((index, solution), ()) => {
        let goals = state.goals->Array.keep(goal => goal.index == index);
        switch (goals[0]) {
        | None => resolved(state)
        | Some(goal) =>
          EditM.modifyGoal(goal, _ => solution, state)
          ->flatMap(sendRequest(Request.Give(goal)))
        };
      };

      // solve them one by one
      let size = Array.length(solutions);
      if (size == 0) {
        ViewM.displayError("No solutions found", None, state);
      } else {
        solutions
        ->Array.map(solveOne)
        ->Util.oneByOne
        ->map(_ => state)
        ->flatMap(
            ViewM.displaySuccess(
              string_of_int(size) ++ " goals solved",
              None,
            ),
          );
      };
    | DisplayInfo(info) => DisplayInfo.handle(info, state)
    | RunningInfo(_verbosity, message) =>
      ViewM.display("Type-checking", Some(message), state)
    | _ => resolved(state)
    };
  };
};
