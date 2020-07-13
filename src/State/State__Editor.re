module Js' = Js;
open! Promise;
module Js = Js';

module Impl = (Editor: Sig.Editor) => {
  module State = State.Impl(Editor);
  // module Task = Task.Impl(Editor);
  module Goal = Goal.Impl(Editor);
  module ViewM = State__View.Impl(Editor);
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

  let saveEditor = (state: State.t) =>
    Editor.save(state.editor)->map(_ => state);

  //////////////////////////////////////////////////////////////////////////////
  // Cursor-related
  //////////////////////////////////////////////////////////////////////////////

  let saveCursor = (state: State.t) => {
    let position = Editor.getCursorPosition(state.editor);
    let offset = Editor.offsetAtPoint(state.editor, position);
    state.cursor = Some(offset);
    resolved(state);
  };

  let restoreCursor = (state: State.t) => {
    switch (state.cursor) {
    | None => ()
    | Some(offset) =>
      state.cursor = None;

      let position = Editor.pointAtOffset(state.editor, offset);

      let pointedGoal = pointingAt(~cursor=offset, state);
      switch (pointedGoal) {
      | Some(goal) =>
        if (Goal.getContent(goal, state.editor) == "") {
          Goal.setCursor(goal, state.editor);
        } else {
          Editor.setCursorPosition(state.editor, position);
        }
      | None =>
        Editor.setCursorPosition(state.editor, position);
        // put the focus back on the editor
        state.editor->Editor.focus;
      };
    };
    resolved(state);
  };

  let setCursor = (offset, state: State.t) => {
    let point = Editor.pointAtOffset(state.editor, offset);
    Editor.setCursorPosition(state.editor, point);
    resolved(state);
  };
  //////////////////////////////////////////////////////////////////////////////
  // Goal-related
  //////////////////////////////////////////////////////////////////////////////

  let instantiateGoals = (indices, state: State.t) => {
    // destroy all existing goals
    state.goals->Array.forEach(Goal.destroy);
    // instantiate new ones
    Goal.makeMany(state.editor, indices)
    ->Promise.map(goals => {
        state.goals = goals;
        state;
      });
  };

  let nextGoal = (state: State.t) => {
    Goal.updateRanges(state.goals, state.editor);
    let nextGoal = ref(None);
    let cursorOffset =
      Editor.offsetAtPoint(
        state.editor,
        Editor.getCursorPosition(state.editor),
      );
    let offsets = getOffsets(state);

    // find the first Goal after the cursor
    offsets->Array.forEach(offset =>
      if (cursorOffset < offset && nextGoal^ === None) {
        nextGoal := Some(offset);
      }
    );

    // if there's no Goal after the cursor, then loop back and return the first Goal
    if (nextGoal^ === None) {
      nextGoal := offsets[0];
    };

    switch (nextGoal^) {
    | None => resolved(state)
    | Some(offset) =>
      Editor.setCursorPosition(
        state.editor,
        Editor.pointAtOffset(state.editor, offset),
      );
      resolved(state);
    };
  };

  let previousGoal = (state: State.t) => {
    Goal.updateRanges(state.goals, state.editor);
    let previousGoal = ref(None);
    let cursorOffset =
      Editor.offsetAtPoint(
        state.editor,
        Editor.getCursorPosition(state.editor),
      );
    let offsets = getOffsets(state);

    // find the last Goal before the cursor
    offsets->Array.forEach(offset =>
      if (cursorOffset > offset) {
        previousGoal := Some(offset);
      }
    );

    // loop back if this is already the first Goal
    if (previousGoal^ === None) {
      previousGoal := offsets[Array.length(offsets) - 1];
    };

    switch (previousGoal^) {
    | None => resolved(state)
    | Some(offset) =>
      Editor.setCursorPosition(
        state.editor,
        Editor.pointAtOffset(state.editor, offset),
      );
      resolved(state);
    };
  };

  let modifyGoal = (goal, f, state: State.t) => {
    Goal.updateRanges(state.goals, state.editor);
    let content = Goal.getContent(goal, state.editor);
    Js.log(
      "[ goal ][ modify ] \"" ++ content ++ "\" => \"" ++ f(content) ++ "\"",
    );
    Goal.setContent(goal, state.editor, f(content))
    ->Promise.flatMap(
        fun
        | true => resolved(state)
        | false =>
          ViewM.displayError(
            "Goal-related Error",
            Some(
              "Failed to modify the content of goal #"
              ++ string_of_int(goal.index),
            ),
            state,
          ),
      );
  };

  let removeBoundaryAndDestroyGoal = (goal, state: State.t) => {
    Goal.updateRanges(state.goals, state.editor);

    let innerRange = Goal.getInnerRange(goal, state.editor);
    let outerRange =
      Editor.Range.make(
        Editor.pointAtOffset(state.editor, fst(goal.range)),
        Editor.pointAtOffset(state.editor, snd(goal.range)),
      );
    let content =
      Editor.getTextInRange(state.editor, innerRange)->String.trim;
    Editor.setText(state.editor, outerRange, content)
    ->Promise.flatMap(
        fun
        | true => {
            Goal.destroy(goal);
            resolved(state);
          }
        | false =>
          ViewM.displayError(
            "Goal-related Error",
            Some(
              "Unable to remove the boundary of goal #"
              ++ string_of_int(goal.index),
            ),
            state,
          ),
      );
  };

  // replace and insert one or more lines of content at the goal
  // usage: case split
  let replaceWithLines = (goal, lines, state: State.t) => {
    // get the width of indentation from the first line of the goal
    let (indentWidth, _, _) = indentationWidth(goal, state.editor);
    let indentation = Js.String.repeat(indentWidth, " ");
    let indentedLines =
      indentation ++ Js.Array.joinWith("\n" ++ indentation, lines);
    Js.log("[ReplaceWithLines]");
    Js.log(indentedLines);
    Js.log("=================");
    // the rows spanned by the goal (including the text outside the goal)
    // will be replaced by the `indentedLines`
    let start = Editor.pointAtOffset(state.editor, fst(goal.range));
    let startLineNo = Editor.Point.line(start);
    let startLineRange = Editor.rangeForLine(state.editor, startLineNo);
    let start = Editor.Range.start(startLineRange);

    let end_ = Editor.pointAtOffset(state.editor, snd(goal.range));
    let rangeToBeReplaced = Editor.Range.make(start, end_);
    Js.log("[rangeToBeReplaced]");
    Js.log(rangeToBeReplaced);
    Js.log("=================");
    Editor.setText(state.editor, rangeToBeReplaced, indentedLines)
    ->Promise.flatMap(
        fun
        | true => {
            Goal.destroy(goal);
            resolved(state);
          }
        | false =>
          ViewM.displayError(
            "Goal-related Error",
            Some(
              "Unable to replace the lines of goal #"
              ++ string_of_int(goal.index),
            ),
            state,
          ),
      );
  };

  // Replace definition of extended lambda with new clauses
  //
  // We are asked to replace a clause like "x → ?" with multiple clauese
  // However, there are two kinds of syntax
  //  1.  λ { x → ?
  //        ; y → ?
  //        }
  //  2.  λ where
  //          x → ?
  //          y → ?
  // We will need to determine whether semicolons ";"
  // should to be inserted in between the given clauses
  // We employ the strategy implemented by the Emacs mode
  // https://github.com/agda/agda/blob/f46ecaf729c00217efad7a77e5d9932bfdd030e5/src/data/emacs-mode/agda2-mode.el#L950
  // which searches backward (starting from the goal) and look for the presence of "{"
  let replaceWithLambda = (goal: Goal.t, lines, state: State.t) => {
    let goalEnd = Editor.pointAtOffset(state.editor, snd(goal.range));
    let (indentWidth, textBeforeGoal, range) =
      indentationWidth(goal, state.editor);
    // start at the last ";", or the first non-blank character
    let scanRow = Editor.Point.line(Editor.Range.start(range));
    let scanColStart =
      switch (Js.String.lastIndexOf(";", textBeforeGoal)) {
      | (-1) => indentWidth
      | n => n + 1
      };
    let scanColEnd = Editor.Point.column(Editor.Range.end_(range));

    // determine the position of "{"
    // iterate from the end to the start
    let bracketCount = ref(0);
    let i = ref(scanColEnd - 1);
    while (i^ >= scanColStart && bracketCount^ >= 0) {
      switch (i^) {
      // no preceding character
      | 0 => ()
      // has preceding character
      | i' =>
        switch (Js.String.charAt(i' - 1, textBeforeGoal)) {
        | "}" => bracketCount := bracketCount^ + 1
        | "{" => bracketCount := bracketCount^ - 1
        | _ => ()
        }
      };
      i := i^ - 1;
    };
    Js.log((textBeforeGoal, i^ + 1, indentWidth));
    let rewriteRange =
      Editor.Range.make(Editor.Point.make(scanRow, i^ + 1), goalEnd);
    let isLambdaWhere = i^ + 1 == indentWidth;
    let rewriteText =
      if (isLambdaWhere) {
        Js.Array.joinWith("\n" ++ Js.String.repeat(indentWidth, " "), lines);
      } else {
        " "
        ++ Js.Array.joinWith(
             "\n" ++ Js.String.repeat(indentWidth, " ") ++ "; ",
             lines,
           );
      };
    Editor.setText(state.editor, rewriteRange, rewriteText)
    ->Promise.flatMap(
        fun
        | true => {
            Goal.destroy(goal);
            resolved(state);
          }
        | false =>
          ViewM.displayError(
            "Goal-related Error",
            Some(
              "Unable to replace the lines of goal #"
              ++ string_of_int(goal.index),
            ),
            state,
          ),
      );
  };

  let localOrGlobal = (local, global, state: State.t): Promise.t(State.t) => {
    Goal.updateRanges(state.goals, state.editor);
    switch (pointingAt(state)) {
    | None => global
    | Some(goal) => local(goal)
    };
  };

  let localOrGlobal2 =
      (local, localEmpty, global, state: State.t): Promise.t(State.t) => {
    switch (pointingAt(state)) {
    | None => global
    | Some(goal) =>
      let content = Goal.getContent(goal, state.editor);
      if (content == "") {
        localEmpty(goal);
      } else {
        local(goal, content);
      };
    };
  };
};
