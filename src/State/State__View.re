module Js' = Js;
open! Promise;
module Js = Js';

module Impl = (Editor: Sig.Editor) => {
  module State = State.Impl(Editor);
  // open Belt;

  let sendEvent = (event: View.EventToView.t, state: State.t) => {
    state->State.sendEventToView(event)->map(_ => state);
  };

  // helper functions
  let display' = header =>
    fun
    | None => sendEvent(Display(header, Nothing))
    | Some(message) => sendEvent(Display(header, Plain(message)));
  let display = header => display'(Plain(header));
  let displayError = header => display'(Error(header));
  let displayWarning = header => display'(Warning(header));
  let displaySuccess = header => display'(Success(header));
};
