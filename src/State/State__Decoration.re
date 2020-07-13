module Js' = Js;
open! Promise;
module Js = Js';

module Impl = (Editor: Sig.Editor) => {
  module State = State.Impl(Editor);
  module Goal = Goal.Impl(Editor);
  module Decoration = Decoration.Impl(Editor);
  open Belt;

  let removeAll = (state: State.t) => {
    state.decorations->Array.forEach(Editor.Decoration.destroy);
    state.decorations = [||];
    Promise.resolved(state);
  };

  let add = (annotations, state: State.t) => {
    annotations->Array.forEach(highlighting => {
      let decorations =
        Decoration.decorateHighlighting(state.editor, highlighting);
      state.decorations = Array.concat(state.decorations, decorations);
    });
    Promise.resolved(state);
  };
};
