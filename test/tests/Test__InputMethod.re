open! BsMocha.Mocha;
open! Belt;

module Impl = (Editor: Sig.Editor) => {
  module Test__Util = Test__Util.Impl(Editor);
  open Test__Util;
  module Goal = Goal.Impl(Editor);
  module Task = Task.Impl(Editor);
  module EditorIM = EditorIM.Impl(Editor);
  module GoalHandler = Handle__Goal.Impl(Editor);

  module Js' = Js;
  open Promise;
  module Js = Js';

  type setup = {
    editor: Editor.editor,
    emitter: Event.t(EditorIM.event),
  };

  let activateExtension = (fileName): Promise.t(setup) => {
    module Main = Main.Impl(Editor);

    let disposables = [||];
    let extensionPath = Path.extensionPath();
    let emitter = Main.activateWithoutContext(disposables, extensionPath);
    Editor.openEditor(fileName)->map(editor => {editor, emitter});
  };

  let acquire = setup => {
    switch (setup^) {
    | None => resolved(Error(Util.Error("Cannot acquire the setup")))
    | Some(setup) => resolved(Ok(setup))
    };
  };

  let cleanup = setup => {
    let range =
      Editor.Range.make(Editor.Point.make(0, 0), Editor.Point.make(100, 0));
    setup.editor->Editor.getDocument->Editor.replaceText(range, "");
    // ->map(_ => Ok());
  };

  let insertChar = (setup, char) => {
    let promise = setup.emitter.once();
    let positions = Editor.getCursorPositions(setup.editor);
    setup.editor
    ->Editor.getDocument
    ->Editor.insertTexts(positions, char)
    ->flatMap(_ => promise)
    ->map(x => Ok(x));
  };

  let backspace = setup => {
    let promise = setup.emitter.once();
    let end_ = Editor.getCursorPosition(setup.editor);
    let start = end_->Editor.Point.translate(0, -1);
    let range = Editor.Range.make(start, end_);
    setup.editor
    ->Editor.getDocument
    ->Editor.deleteText(range)
    ->flatMap(_ => promise)
    ->map(x => Ok(x));
  };

  module IM = {
    let activate = (setup, ~positions=?, ()) => {
      let promise = setup.emitter.once();
      let positions =
        positions->Option.getWithDefault([|
          Editor.getCursorPosition(setup.editor),
        |]);
      setup.editor->Editor.setCursorPositions(positions);
      VSCode.Commands.executeCommand0("agda-mode.input-symbol[Activate]")
      ->flatMap(result => result)
      ->flatMap(_ => promise)
      ->map(x => Ok(x));
      // document->Editor.insertTexts(points, "\\")->flatMap(_ => promise);
    };

    let deactivate = setup => {
      let promise = setup.emitter.once();
      VSCode.Commands.executeCommand0("agda-mode.escape")
      ->flatMap(result => result)
      ->flatMap(_ => promise)
      ->map(x => Ok(x));
    };
  };

  describe("Input Method (Editor)", () => {
    let setup = ref(None);

    Q.before(() => {
      activateExtension(Path.asset("InputMethod.agda"))
      ->map(value => {
          setup := Some(value);
          Ok();
        })
    });

    Q.after_each(() => {acquire(setup)->mapOk(cleanup)});

    describe("Insertion", () => {
      Q.it({j|should translate "lambdabar" to "λ"|j}, () => {
        acquire(setup)
        ->flatMapOk(setup => {
            let document = Editor.getDocument(setup.editor);
            IM.activate(setup, ())
            ->flatMapOk(A.equal(EditorIM.Activate))
            ->flatMapOk(() => insertChar(setup, "l"))
            ->flatMapOk(A.equal(EditorIM.Change))
            ->flatMapOk(() => A.equal({j|←|j}, Editor.getText(document)))
            ->flatMapOk(() => insertChar(setup, "a"))
            ->flatMapOk(A.equal(EditorIM.Change))
            ->flatMapOk(() => A.equal({j|←a|j}, Editor.getText(document)))
            ->flatMapOk(() => insertChar(setup, "m"))
            ->flatMapOk(A.equal(EditorIM.Change))
            ->flatMapOk(() => A.equal({j|←am|j}, Editor.getText(document)))
            ->flatMapOk(() => insertChar(setup, "b"))
            ->flatMapOk(A.equal(EditorIM.Change))
            ->flatMapOk(() =>
                A.equal({j|←amb|j}, Editor.getText(document))
              )
            ->flatMapOk(() => insertChar(setup, "d"))
            ->flatMapOk(A.equal(EditorIM.Change))
            ->flatMapOk(() =>
                A.equal({j|←ambd|j}, Editor.getText(document))
              )
            ->flatMapOk(() => insertChar(setup, "a"))
            ->flatMapOk(A.equal(EditorIM.Change))
            ->flatMapOk(() => A.equal({j|λ|j}, Editor.getText(document)))
            ->flatMapOk(() => insertChar(setup, "b"))
            ->flatMapOk(A.equal(EditorIM.Change))
            ->flatMapOk(() => A.equal({j|λb|j}, Editor.getText(document)))
            ->flatMapOk(() => insertChar(setup, "a"))
            ->flatMapOk(A.equal(EditorIM.Change))
            ->flatMapOk(() => A.equal({j|λba|j}, Editor.getText(document)))
            ->flatMapOk(() => insertChar(setup, "r"))
            ->flatMapOk(A.equal(EditorIM.Change))
            ->flatMapOk(() => A.equal({j|ƛ|j}, Editor.getText(document)));
          })
      });
      Q.it({j|should translate "bn" to "𝕟"|j}, () => {
        acquire(setup)
        ->flatMapOk(setup => {
            let document = Editor.getDocument(setup.editor);
            IM.activate(setup, ())
            ->flatMapOk(A.equal(EditorIM.Activate))
            ->flatMapOk(() => insertChar(setup, "b"))
            ->flatMapOk(A.equal(EditorIM.Change))
            ->flatMapOk(() => {A.equal({j|♭|j}, Editor.getText(document))})
            ->flatMapOk(() => insertChar(setup, "n"))
            ->flatMapOk(A.equal(EditorIM.Change))
            ->flatMapOk(() => {
                A.equal({j|𝕟|j}, Editor.getText(document))
              });
            // ->flatMapOk(() => IM.deactivate(setup))
            // ->flatMapOk(A.equal(EditorIM.Deactivate))
          })
      });
    });
    describe("Backspacing", () => {
      Q.it({j|should work just fine|j}, () => {
        acquire(setup)
        ->flatMapOk(setup => {
            let document = Editor.getDocument(setup.editor);
            IM.activate(setup, ())
            ->flatMapOk(A.equal(EditorIM.Activate))
            ->flatMapOk(() => insertChar(setup, "l"))
            ->flatMapOk(A.equal(EditorIM.Change))
            ->flatMapOk(() => A.equal({j|←|j}, Editor.getText(document)))
            ->flatMapOk(() => insertChar(setup, "a"))
            ->flatMapOk(A.equal(EditorIM.Change))
            ->flatMapOk(() => A.equal({j|←a|j}, Editor.getText(document)))
            ->flatMapOk(() => insertChar(setup, "m"))
            ->flatMapOk(A.equal(EditorIM.Change))
            ->flatMapOk(() => A.equal({j|←am|j}, Editor.getText(document)))
            ->flatMapOk(() => insertChar(setup, "b"))
            ->flatMapOk(A.equal(EditorIM.Change))
            ->flatMapOk(() =>
                A.equal({j|←amb|j}, Editor.getText(document))
              )
            ->flatMapOk(() => insertChar(setup, "d"))
            ->flatMapOk(A.equal(EditorIM.Change))
            ->flatMapOk(() =>
                A.equal({j|←ambd|j}, Editor.getText(document))
              )
            ->flatMapOk(() => insertChar(setup, "a"))
            ->flatMapOk(A.equal(EditorIM.Change))
            ->flatMapOk(() => A.equal({j|λ|j}, Editor.getText(document)))
            ->flatMapOk(() => backspace(setup))
            ->flatMapOk(A.equal(EditorIM.Change))
            ->flatMapOk(() => {
                A.equal({j|lambd|j}, Editor.getText(document))
              })
            ->flatMapOk(() => IM.deactivate(setup))
            ->flatMapOk(A.equal(EditorIM.Deactivate));
          })
      })
    });
    describe("Multiple cursors at once", () => {
      let positions = [|
        Editor.Point.make(0, 0),
        Editor.Point.make(1, 0),
        Editor.Point.make(2, 0),
        Editor.Point.make(3, 0),
      |];
      Q.it({j|should work just fine|j}, () => {
        let replaceCRLF = Js.String.replaceByRe([%re "/\\r\\n/g"], "\n");

        acquire(setup)
        ->flatMapOk(setup => {
            let document = Editor.getDocument(setup.editor);

            document
            ->Editor.insertText(Editor.Point.make(0, 0), "\n\n\n")
            ->flatMap(_ => IM.activate(setup, ~positions, ()))
            ->flatMapOk(A.equal(EditorIM.Activate))
            ->flatMapOk(() => insertChar(setup, "b"))
            ->flatMapOk(A.equal(EditorIM.Change))
            ->flatMapOk(() =>
                A.equal(
                  {j|♭\n♭\n♭\n♭|j},
                  replaceCRLF(Editor.getText(document)),
                )
              )
            ->flatMapOk(() => insertChar(setup, "n"))
            ->flatMapOk(A.equal(EditorIM.Change))
            ->flatMapOk(() =>
                A.equal(
                  {j|𝕟\n𝕟\n𝕟\n𝕟|j},
                  replaceCRLF(Editor.getText(document)),
                )
              );
          });
      });
    });
  });
};

include Impl(SigImpl);
