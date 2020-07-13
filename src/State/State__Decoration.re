module Js' = Js;
open! Promise;
module Js = Js';

module Impl = (Editor: Sig.Editor) => {
  module State = State.Impl(Editor);
  module Goal = Goal.Impl(Editor);
  open Belt;

  let removeAll = (state: State.t) => {
    state.decorations->Array.forEach(Editor.Decoration.destroy);
    state.decorations = [||];
    Promise.resolved(state);
  };
};
