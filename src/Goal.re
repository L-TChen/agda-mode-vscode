open Belt;
module Impl = (Editor: Sig.Editor) => {
  module Decoration = Decoration.Impl(Editor);
  type t = {
    index: int,
    mutable range: (int, int),
    decorations: array(Editor.Decoration.t),
  };

  // NOTE: helper function of `makeMany`, returns a thunk
  let make =
      (
        editor: Editor.editor,
        edit: (Editor.Range.t, string) => Promise.t(unit),
        diff: SourceFile.Diff.t,
      )
      : (unit => Promise.t(t)) => {
    // modify the text buffer base on the Diff
    () => {
      let originalRange =
        Editor.Range.make(
          Editor.pointAtOffset(editor, fst(diff.originalRange)),
          Editor.pointAtOffset(editor, snd(diff.originalRange)),
        );
      edit(originalRange, diff.content)
      ->Promise.map(() => {
          let decorations =
            Decoration.decorateHole(editor, diff.modifiedRange, diff.index);
          {index: diff.index, range: diff.modifiedRange, decorations};
        });
    };
  };

  let generateDiffs =
      (editor: Editor.editor, indices: array(int)): array(SourceFile.Diff.t) => {
    let filePath =
      Editor.getFileName(editor)->Option.getWithDefault("unnamed.agda");
    let source = Editor.getText(editor);
    SourceFile.parse(indices, filePath, source);
  };

  // make an array of Goal.t with given goal indices
  // modifies the text buffer along the way
  let makeMany =
      (
        editor: Editor.editor,
        edit: (Editor.Range.t, string) => Promise.t(unit),
        indices: array(int),
      )
      : Promise.t(array(t)) => {
    let diffs = generateDiffs(editor, indices);
    // scan through the diffs to modify the text buffer one by one
    diffs->Array.map(make(editor, edit))->Util.oneByOne;
  };

  // parse the whole source file and update the ranges of an array of Goal.t
  let updateRanges = (goals: array(t), editor: Editor.editor) => {
    let indices = goals->Array.map(goal => goal.index);
    let diffs = generateDiffs(editor, indices);
    diffs->Array.forEachWithIndex((i, diff) => {
      switch (goals[i]) {
      | None => () // do nothing :|
      | Some(goal) => goal.range = diff.modifiedRange
      }
    });
  };

  let getInnerRange = (self, editor) =>
    Editor.Range.make(
      Editor.pointAtOffset(editor, fst(self.range) + 2),
      Editor.pointAtOffset(editor, snd(self.range) - 2),
    );

  let getContent = (self, editor) => {
    let innerRange = getInnerRange(self, editor);
    Editor.getTextInRange(editor, innerRange)->Parser.userInput;
  };

  let setContent =
      (self, editor, edit: (Editor.Range.t, string) => Promise.t(unit), text) => {
    let innerRange = getInnerRange(self, editor);
    edit(innerRange, " " ++ text ++ " ");
  };

  let setCursor = (self, editor) => {
    let (start, _) = self.range;
    let point = Editor.pointAtOffset(editor, start + 3);
    Editor.setCursorPosition(editor, point);
  };

  let buildHaskellRange = (editor, self, old, filepath: string) => {
    let (start, end_) = self.range;
    let startPoint = Editor.pointAtOffset(editor, start);
    let endPoint = Editor.pointAtOffset(editor, end_);

    let startIndex = string_of_int(start + 3);
    let startRow = string_of_int(Editor.Point.line(startPoint) + 1);
    let startColumn = string_of_int(Editor.Point.column(startPoint) + 3);
    let startPart = {j|$(startIndex) $(startRow) $(startColumn)|j};
    let endIndex' = string_of_int(end_ - 3);
    let endRow = string_of_int(Editor.Point.line(endPoint) + 1);
    let endColumn = string_of_int(Editor.Point.column(endPoint) - 1);
    let endPart = {j|$(endIndex') $(endRow) $(endColumn)|j};

    if (old) {
      {j|(Range [Interval (Pn (Just (mkAbsolute "$(filepath)")) $(startPart)) (Pn (Just (mkAbsolute "$(filepath)")) $(endPart))])|j}
      // before (not including) 2.5.1
    } else {
      {j|(intervalsToRange (Just (mkAbsolute "$(filepath)")) [Interval (Pn () $(startPart)) (Pn () $(endPart))])|j}
      // after 2.5.1
    };
  };

  let destroy = self => {
    self.decorations->Array.forEach(Editor.Decoration.destroy);
  };
};
