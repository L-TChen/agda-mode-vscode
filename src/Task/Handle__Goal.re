open Belt;

module Impl = (Editor: Sig.Editor) => {
  module Task = Task.Impl(Editor);
  module State = State.Impl(Editor);
  module Goal = Goal.Impl(Editor);

  open! Task;

  // return an array of Offsets of Goals
  let getOffsets = (state: State.t): array(int) => {
    state.goals->Array.map(goal => fst(goal.range) + 3);
  };

  let pointingAt = (~cursor=?, state: State.t): option(Goal.t) => {
    let cursor =
      switch (cursor) {
      | None => Editor.getCursorPosition(state.editor)
      | Some(x) => x
      };
    let cursorOffset =
      Editor.offsetAtPoint(Editor.getDocument(state.editor), cursor);
    let pointedGoals =
      state.goals
      ->Array.keep(goal =>
          fst(goal.range) <= cursorOffset && cursorOffset <= snd(goal.range)
        );
    // return the first pointed goal
    pointedGoals[0];
  };

  // There are two kinds of lambda abstraction
  //  1.  λ { x → ? }
  //  2.  λ where x → ?
  // We employ the strategy implemented by the Emacs mode
  // https://github.com/agda/agda/blob/f46ecaf729c00217efad7a77e5d9932bfdd030e5/src/data/emacs-mode/agda2-mode.el#L950
  // which searches backward (starting from the goal) and see if there's a open curly bracket "{"
  //
  //      λ { x → ? }
  //        ^------------------- open curly bracket
  //
  //  or if there's a "where" token
  //
  //      λ where x → ?
  //        ^------------------- the "where" token
  //
  let caseSplitAux = (editor, goal: Goal.t) => {
    let textBeforeGoal = {
      let start = Editor.pointAtOffset(editor, 0);
      let end_ = Editor.pointAtOffset(editor, fst(goal.range));
      let range = Editor.Range.make(start, end_);
      Editor.getTextInRange(editor, range);
    };

    // returns the offset of the first non-blank character it encountered from somewhere
    let nextWordBoundary = (start, string) => {
      let break = ref(false);
      let n = ref(0);

      let i = ref(start);
      while (i^ < Js.String.length(string) && ! break^) {
        let char = Js.String.charAt(i^, string);
        switch (char) {
        // skip blank characters
        | " "
        | "\012"
        | "\t" => n := n^ + 1
        // stop when we hit something
        | _ => break := true
        };
        i := i^ + 1;
      };
      start + n^;
    };

    let (inWhereClause, searchStart, lastLineBreakOffset) = {
      let lastOpenCurlyBracketOffset =
        {
          let bracketCount = ref(0);
          let i = ref(fst(goal.range) - 1);
          while (i^ >= 0 && bracketCount^ >= 0) {
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
            // scanning backwards
            i := i^ - 1;
          };
          i^;
        }
        + 1;

      let lastSemicolonOffset =
        (
          switch (Js.String.lastIndexOf(";", textBeforeGoal)) {
          | (-1) => 0
          | n => n
          }
        )
        + 1;

      let lastWhereTokenOffset =
        (
          switch (Js.String.lastIndexOf("where", textBeforeGoal)) {
          | (-1) => 0
          | n => n
          }
        )
        + 5;

      let lastLineBreakOffset =
        max(
          0,
          max(
            Js.String.lastIndexOf("\r", textBeforeGoal),
            Js.String.lastIndexOf("\n", textBeforeGoal),
          ),
        )
        + 1;

      let inWhereClause = lastWhereTokenOffset > lastOpenCurlyBracketOffset;
      let offset =
        max(
          max(lastLineBreakOffset, lastSemicolonOffset),
          max(lastWhereTokenOffset, lastOpenCurlyBracketOffset),
        );
      (inWhereClause, offset, lastLineBreakOffset);
    };

    // returns the range of the case clause (that is going to be replaced)
    //    "x -> {!   !}"
    let range = {
      let caseStart = nextWordBoundary(searchStart, textBeforeGoal);
      let caseEnd = snd(goal.range);
      (caseStart, caseEnd);
    };
    (inWhereClause, fst(range) - lastLineBreakOffset, range);
  };

  // returns the width of indentation of the first line of a goal
  // along with the text and the range before the goal
  let indentationWidth =
      (editor, goal: Goal.t): (int, string, Editor.Range.t) => {
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

  // from Goal-related action to Tasks
  let handle =
    fun
    | Goal.Instantiate(indices) => [
        Task.WithStateP(
          state => {
            // destroy all existing goals
            state.goals->Array.forEach(Goal.destroy);
            // instantiate new ones
            Goal.makeMany(state.editor, indices)
            ->Promise.map(goals => {
                state.goals = goals;
                [];
              });
          },
        ),
      ]
    | UpdateRange => [
        WithState(
          state => {
            Goal.updateRanges(state.goals, Editor.getDocument(state.editor))
          },
        ),
      ]
    | Next => [
        Goal(UpdateRange),
        WithStateP(
          state => {
            let document = Editor.getDocument(state.editor);
            let nextGoal = ref(None);
            let cursorOffset =
              Editor.offsetAtPoint(
                document,
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
            | None => Promise.resolved([])
            | Some(offset) =>
              Editor.setCursorPosition(
                state.editor,
                Editor.pointAtOffset(document, offset),
              );
              Promise.resolved([Goal(JumpToOffset(offset))]);
            };
          },
        ),
      ]
    | Previous => [
        Goal(UpdateRange),
        WithStateP(
          state => {
            let document = Editor.getDocument(state.editor);
            let previousGoal = ref(None);
            let cursorOffset =
              Editor.offsetAtPoint(
                document,
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
            | None => Promise.resolved([])
            | Some(offset) =>
              Editor.setCursorPosition(
                state.editor,
                Editor.pointAtOffset(document, offset),
              );
              Promise.resolved([Goal(JumpToOffset(offset))]);
            };
          },
        ),
      ]
    | Modify(goal, f) => [
        Goal(UpdateRange),
        WithStateP(
          state => {
            let document = Editor.getDocument(state.editor);
            let content = Goal.getContent(goal, document);
            Goal.setContent(goal, document, f(content))
            ->Promise.map(
                fun
                | true => []
                | false => [
                    display(
                      Error("Goal-related Error"),
                      Plain(
                        "Failed to modify the content of goal #"
                        ++ string_of_int(goal.index),
                      ),
                    ),
                  ],
              );
          },
        ),
      ]

    | SaveCursor =>
      // WithState(
      //   state => {
      //     let position = Editor.getCursorPosition(state.editor);
      //     state.cursor = Some(position);
      //   },
      // ),
      []
    //  if the cursor is pointing at the boundary of some hole
    //    then move the cursor inside the hole
    //    else restore the cursor to its original position (if there's any)
    | RestoreCursor =>
      // WithState(
      //   state => {
      //     switch (state.cursor) {
      //     | None => ()
      //     | Some(position) =>
      //       state.cursor = None;
      //       let cursor = Editor.getCursorPosition(state.editor);
      //       let cursorOffset = Editor.offsetAtPoint(state.editor, cursor);
      //       let pointedGoals =
      //         state.goals
      //         ->Array.keep(goal =>
      //             fst(goal.range) <= cursorOffset
      //             && cursorOffset <= fst(goal.range)
      //             + 2
      //             || snd(goal.range)
      //             - 2 <= cursorOffset
      //             && cursorOffset <= snd(goal.range)
      //           );
      //       let pointedGoal = pointedGoals[0];
      //       switch (pointedGoal) {
      //       | Some(goal) => Goal.setCursor(goal, state.editor)
      //       | None =>
      //         Editor.setCursorPosition(state.editor, position);
      //         // put the focus back on the editor
      //         state.editor->Editor.focus;
      //       };
      //     }
      //   },
      // ),
      []
    | SetCursor(offset) => [
        WithState(
          state => {
            let document = Editor.getDocument(state.editor);
            let point = Editor.pointAtOffset(document, offset);
            Editor.setCursorPosition(state.editor, point);
          },
        ),
      ]
    | JumpToOffset(offset) => [
        WithState(
          state => {
            let document = Editor.getDocument(state.editor);
            let point = Editor.pointAtOffset(document, offset);
            let range = Editor.Range.make(point, point);
            Editor.reveal(state.editor, range);
          },
        ),
      ]
    | RemoveBoundaryAndDestroy(goal) => [
        Goal(UpdateRange),
        WithStateP(
          state => {
            let document = Editor.getDocument(state.editor);
            let innerRange = Goal.getInnerRange(goal, document);
            let outerRange =
              Editor.Range.make(
                Editor.pointAtOffset(document, fst(goal.range)),
                Editor.pointAtOffset(document, snd(goal.range)),
              );
            let content =
              Editor.getTextInRange(document, innerRange)->String.trim;
            Editor.replaceText(document, outerRange, content)
            ->Promise.map(
                fun
                | true => {
                    Goal.destroy(goal);
                    [];
                  }
                | false => [
                    display(
                      Error("Goal-related Error"),
                      Plain(
                        "Unable to remove the boundary of goal #"
                        ++ string_of_int(goal.index),
                      ),
                    ),
                  ],
              );
          },
        ),
      ]
    // replace and insert one or more lines of content at the goal
    // usage: case split
    | ReplaceWithLines(goal, lines) => [
        WithStateP(
          state => {
            let document = Editor.getDocument(state.editor);
            // get the width of indentation from the first line of the goal
            let (indentWidth, _, _) = indentationWidth(document, goal);
            let indentation = Js.String.repeat(indentWidth, " ");
            let indentedLines =
              indentation ++ Js.Array.joinWith("\n" ++ indentation, lines);
            // the rows spanned by the goal (including the text outside the goal)
            // will be replaced by the `indentedLines`
            let start = Editor.pointAtOffset(document, fst(goal.range));
            let startLineNo = Editor.Point.line(start);
            let startLineRange = Editor.rangeForLine(document, startLineNo);
            let start = Editor.Range.start(startLineRange);

            let end_ = Editor.pointAtOffset(document, snd(goal.range));
            let rangeToBeReplaced = Editor.Range.make(start, end_);
            Editor.replaceText(document, rangeToBeReplaced, indentedLines)
            ->Promise.map(
                fun
                | true => {
                    Goal.destroy(goal);
                    [];
                  }
                | false => [
                    display(
                      Error("Goal-related Error"),
                      Plain(
                        "Unable to replace the lines of goal #"
                        ++ string_of_int(goal.index),
                      ),
                    ),
                  ],
              );
          },
        ),
      ]

    // Replace definition of extended lambda with new clauses
    //
    // We are asked to replace a clause like "x → ?" with multiple clauese
    // There are two kinds of syntax for extended lambda
    //  1.  λ { x → ?
    //        ; y → ?
    //        }
    //  2.  λ where
    //          x → ?
    //          y → ?
    | ReplaceWithLambda(goal, lines) => [
        WithStateP(
          state => {
            let document = Editor.getDocument(state.editor);
            let (inWhereClause, indentWidth, rewriteRange) =
              caseSplitAux(document, goal);
            let rewriteText =
              if (inWhereClause) {
                Js.Array.joinWith(
                  "\n" ++ Js.String.repeat(indentWidth, " "),
                  lines,
                );
              } else {
                Js.Array.joinWith(
                  "\n" ++ Js.String.repeat(indentWidth - 2, " ") ++ "; ",
                  lines,
                );
              };

            let rewriteRange =
              Editor.Range.make(
                Editor.pointAtOffset(document, fst(rewriteRange)),
                Editor.pointAtOffset(document, snd(rewriteRange)),
              );
            Editor.replaceText(document, rewriteRange, rewriteText)
            ->Promise.map(
                fun
                | true => {
                    Goal.destroy(goal);
                    [];
                  }
                | false => [
                    display(
                      Error("Goal-related Error"),
                      Plain(
                        "Unable to replace the lines of goal #"
                        ++ string_of_int(goal.index),
                      ),
                    ),
                  ],
              );
          },
        ),
      ]
    | LocalOrGlobal2(local, localEmpty, global) => [
        Goal(UpdateRange),
        WithStateP(
          state => {
            let document = Editor.getDocument(state.editor);
            switch (pointingAt(state)) {
            | None => Promise.resolved(global)
            | Some(goal) =>
              let content = Goal.getContent(goal, document);
              if (content == "") {
                Promise.resolved(localEmpty(goal));
              } else {
                Promise.resolved(local(goal, content));
              };
            };
          },
        ),
      ]
    | LocalOrGlobal(local, global) => [
        Goal(UpdateRange),
        WithStateP(
          state => {
            switch (pointingAt(state)) {
            | None => Promise.resolved(global)
            | Some(goal) => Promise.resolved(local(goal))
            }
          },
        ),
      ];
};
