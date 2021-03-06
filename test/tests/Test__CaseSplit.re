open! BsMocha.Mocha;
module Assert = BsMocha.Assert;
module P = BsMocha.Promise;
open VSCode;
open! Belt;

module Impl = (Editor: Sig.Editor) => {
  module Test__Util = Test__Util.Impl(Editor);
  open Test__Util;

  let makeTextEditor = content =>
    Workspace.openTextDocumentWithOptions(
      Some({"content": content, "language": "agda"}),
    )
    ->Promise.flatMap(textDocument =>
        Window.showTextDocumentWithShowOptions(textDocument, None)
      );

  module Goal = Goal.Impl(Editor);
  module Task = Task.Impl(Editor);
  module Dispatcher = Dispatcher.Impl(Editor);
  module GoalHandler = Handle__Goal.Impl(Editor);

  let forGoal = (goals, index, callback) => {
    switch (goals[index]) {
    | None =>
      Assert.fail("failed to instantiate goal #" ++ string_of_int(index));
      Promise.resolved();
    | Some(goal) => callback(goal)
    };
  };

  describe("Handle__Goal.caseSplitAux", () => {
    // let lines = [|{j|Z + y = ?|j}, {j|S x + y = ?|j}|];
    P.it(
      "should calculate the infomation needed for case splitting correctly", ()
      => {
        Editor.openEditor(Path.asset("CaseSplit.agda"))
        ->Promise.map(editor => {
            let document = Editor.getDocument(editor);
            Goal.makeMany(editor, [|0, 1, 2, 3, 4, 5, 6, 7, 8|])
            ->Promise.map(goals => {
                goals->Array.map(goal => {
                  // convert `rewriteRange` to text in that range because range offsets are different on different OSs
                  let (inWhereClause, indentWidth, rewriteRange) =
                    GoalHandler.caseSplitAux(document, goal);
                  let rewriteRange =
                    Editor.Range.fromOffset(document, rewriteRange);
                  (
                    inWhereClause,
                    indentWidth,
                    Editor.getTextInRange(document, rewriteRange),
                  );
                })
              })
            ->Promise.map(results => {
                Assert.deep_equal(
                  results,
                  [|
                    (false, 9, {j|x → {!   !}|j}),
                    (false, 23, {j|y → {!   !}|j}),
                    (false, 4, {j|x → {!   !}|j}),
                    (false, 4, {j|y → {!   !}|j}),
                    (true, 13, {j|x → {!   !}|j}),
                    (true, 13, {j|y → {!   !}|j}),
                    (true, 2, {j|x → {!   !}|j}),
                    (true, 2, {j|y → {!   !}|j}),
                    (false, 13, {j|x → {!   !}|j}),
                  |],
                )
              });
          })
        ->Promise.Js.toBsPromise
      })
      //   describe("Function", () => {
      //     let lines = [|{j|Z + y = ?|j}, {j|S x + y = ?|j}|];
      //     P.it("without indentation", () => {
      //       let source = {j|
      // _+_ : ℕ → ℕ → ℕ
      // x + y = {!   !}
      // |j};
      //       makeTextEditor(source)
      //       ->Promise.flatMap(editor => {
      //           let dispatcher =
      //             Dispatcher.make(Path.extensionPath(), editor, () => ());
      //           Goal.makeMany(editor, [|0|])
      //           ->Promise.flatMap(goals => {
      //               switch (goals[0]) {
      //               | None =>
      //                 Assert.fail("failed to instantiate any goals");
      //                 Promise.resolved();
      //               | Some(goal) =>
      //                 let tasks =
      //                   GoalHandler.handle(ReplaceWithLines(goal, lines));
      //                 Dispatcher.addToTheBackCritical(dispatcher, tasks)
      //                 ->Promise.map(() => {
      //                     let expected = {j|
      // _+_ : ℕ → ℕ → ℕ
      // Z + y = ?
      // S x + y = ?
      // |j};
      //                     Assert.equal(editor->Editor.getText, expected);
      //                   });
      //               }
      //             });
      //         })
      //       ->Promise.Js.toBsPromise;
      //     });
      //     P.it("with indentation", () => {
      //       let source = {j|
      //   _+_ : ℕ → ℕ → ℕ
      //   x + y = {!   !}
      // |j};
      //       makeTextEditor(source)
      //       ->Promise.flatMap(editor => {
      //           let dispatcher =
      //             Dispatcher.make(Path.extensionPath(), editor, () => ());
      //           Goal.makeMany(editor, [|0|])
      //           ->Promise.flatMap(goals => {
      //               switch (goals[0]) {
      //               | None =>
      //                 Assert.fail("failed to instantiate any goals");
      //                 Promise.resolved();
      //               | Some(goal) =>
      //                 let tasks =
      //                   GoalHandler.handle(ReplaceWithLines(goal, lines));
      //                 Dispatcher.addToTheBackCritical(dispatcher, tasks)
      //                 ->Promise.map(() => {
      //                     let expected = {j|
      //   _+_ : ℕ → ℕ → ℕ
      //   Z + y = ?
      //   S x + y = ?
      // |j};
      //                     Assert.equal(editor->Editor.getText, expected);
      //                   });
      //               }
      //             });
      //         })
      //       ->Promise.Js.toBsPromise;
      //     });
      //   });
      //   describe_only("Extended lambda", () => {
      //     let lines = [|{j|true → ?|j}, {j|false → ?|j}|];
      //     P.it({j|"in curly brackets, newline after λ"|j}, () => {
      //       let source = {j|
      // not : Bool → Bool
      // not = λ
      //     { x → {!   !}}
      // |j};
      //       makeTextEditor(source)
      //       ->Promise.flatMap(editor => {
      //           let dispatcher =
      //             Dispatcher.make(Path.extensionPath(), editor, () => ());
      //           Goal.makeMany(editor, [|0|])
      //           ->Promise.flatMap(goals => {
      //               switch (goals[0]) {
      //               | None =>
      //                 Assert.fail("failed to instantiate any goals");
      //                 Promise.resolved();
      //               | Some(goal) =>
      //                 let (inWhereClause, rewriteRange) =
      //                   GoalHandler.inWhereClause(editor, goal);
      //                 Assert.equal(
      //                   inWhereClause,
      //                   false,
      //                   ~message="shouldn't be in a where clause",
      //                 );
      //                 Assert.deep_equal(
      //                   rewriteRange,
      //                   Editor.Range.make(
      //                     Editor.Point.make(3, 5),
      //                     Editor.Point.make(3, 17),
      //                   ),
      //                 );
      //                 let tasks =
      //                   GoalHandler.handle(ReplaceWithLambda(goal, lines));
      //                 Dispatcher.addToTheBackCritical(dispatcher, tasks)
      //                 ->Promise.map(() => {
      //                     let expected = {j|
      // not : Bool → Bool
      // not = λ
      //     { true → ?
      //     ; false → ?}
      // |j};
      //                     Assert.equal(editor->Editor.getText, expected);
      //                   });
      //               }
      //             });
      //         })
      //       ->Promise.Js.toBsPromise;
      //     });
      //     P.it({j|"in curly brackets, no newline after λ"|j}, () => {
      //       let source = {j|
      // not : Bool → Bool
      // not = λ { x → ? }
      // |j};
      //       makeTextEditor(source)
      //       ->Promise.flatMap(editor => {
      //           let dispatcher =
      //             Dispatcher.make(Path.extensionPath(), editor, () => ());
      //           Goal.makeMany(editor, [|0|])
      //           ->Promise.flatMap(goals => {
      //               switch (goals[0]) {
      //               | None =>
      //                 Assert.fail("failed to instantiate any goals");
      //                 Promise.resolved();
      //               | Some(goal) =>
      //                 let (inWhereClause, rewriteRange) =
      //                   GoalHandler.inWhereClause(editor, goal);
      //                 Assert.equal(
      //                   inWhereClause,
      //                   false,
      //                   ~message="shouldn't be in a where clause",
      //                 );
      //                 Assert.deep_equal(
      //                   rewriteRange,
      //                   Editor.Range.make(
      //                     Editor.Point.make(2, 9),
      //                     Editor.Point.make(2, 21),
      //                   ),
      //                 );
      //                 let tasks =
      //                   GoalHandler.handle(ReplaceWithLambda(goal, lines));
      //                 Dispatcher.addToTheBackCritical(dispatcher, tasks)
      //                 ->Promise.map(() => {
      //                     let expected = {j|
      // not : Bool → Bool
      // not = λ { true → ?
      //         ; false → ?}
      // |j};
      //                     ();
      //                     // Assert.equal(editor->Editor.getText, expected);
      //                   });
      //               }
      //             });
      //         })
      //       ->Promise.Js.toBsPromise;
      //     });
      //   });
  });
};

include Impl(SigImpl);
