module Js' = Js;
open! Promise;
module Js = Js';

module Impl = (Editor: Sig.Editor) => {
  module State = State.Impl(Editor);
  // module Task = Task.Impl(Editor);
  module Goal = Goal.Impl(Editor);
  open Belt;

  // return an array of Offsets of Goals
  let getOffsets = (state: State.t): array(int) => {
    state.goals->Array.map(goal => fst(goal.range) + 3);
  };

  let pointingAt = (~cursor=?, state: State.t): option(Goal.t) => {
    let cursorOffset =
      switch (cursor) {
      | None =>
        Editor.offsetAtPoint(
          state.editor,
          Editor.getCursorPosition(state.editor),
        )
      | Some(x) => x
      };
    let pointedGoals =
      state.goals
      ->Array.keep(goal =>
          fst(goal.range) <= cursorOffset && cursorOffset <= snd(goal.range)
        );
    // return the first pointed goal
    pointedGoals[0];
  };

  // returns the width of indentation of the first line of a goal
  // along with the text and the range before the goal
  let indentationWidth =
      (goal: Goal.t, editor): (int, string, Editor.Range.t) => {
    let goalStart = Editor.pointAtOffset(editor, fst(goal.range));
    let lineNo = Editor.Point.line(goalStart);
    let range = Editor.Range.make(Editor.Point.make(lineNo, 0), goalStart);
    let textBeforeGoal = Editor.getTextInRange(editor, range);
    // tally the number of blank characters
    // ' ', '\012', '\n', '\r', and '\t'
    let indentedBy = s => {
      let n = ref(0);
      for (i in 0 to Js.String.length(s) - 1) {
        switch (Js.String.charAt(i, s)) {
        | " "
        | "\012"
        | "\n"
        | "\r"
        | "\t" =>
          if (i == n^) {
            n := n^ + 1;
          }
        | _ => ()
        };
      };
      n^;
    };
    (indentedBy(textBeforeGoal), textBeforeGoal, range);
  };

  let saveCursor = (state: State.t) => {
    let position = Editor.getCursorPosition(state.editor);
    let offset = Editor.offsetAtPoint(state.editor, position);
    state.cursor = Some(offset);
    Promise.resolved(state);
  };

  let saveEditor = (state: State.t) =>
    Editor.save(state.editor)->Promise.map(_ => state);
};
