module Js' = Js;
open! Promise;
module Js = Js';

module Impl = (Editor: Sig.Editor) => {
  module State = State.Impl(Editor);
  // open Belt;

  let sendEvent = (event: View.EventToView.t, state: State.t) => {
    state->State.sendEventToView(event)->map(_ => state);
  };

  let sendRequest = (request: View.Request.t, callback, state: State.t) => {
    state
    ->State.sendRequestToView(request)
    ->flatMap(
        fun
        | None => resolved(state)
        | Some(response) => callback(response),
      );
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

  let query =
      (
        header,
        placeholder,
        value,
        callbackOnQuerySuccess: string => Promise.t(State.t),
        state: State.t,
      ) => {
    // focus on the panel before inquiring
    Editor.setContext("agdaModeQuerying", true)->ignore;
    state.view->Editor.View.focus;
    Js.log("focus in");
    sendRequest(
      Query(header, placeholder, value),
      response => {
        (
          switch (response) {
          | View.Response.Success => resolved(state)
          | QuerySuccess(result) => callbackOnQuerySuccess(result)
          | QueryInterrupted => displayError("Query Cancelled", None, state)
          }
        )
        ->map(_ => {
            // put the focus back to the editor after inquiring
            Editor.setContext("agdaModeQuerying", false)->ignore;
            state.editor->Editor.focus;
            Js.log("focus back");

            state;
          })
      },
      state,
    );
  };
};
