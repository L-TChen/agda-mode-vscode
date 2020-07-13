module Impl = (Editor: Sig.Editor) => {
  module Goal = Goal.Impl(Editor);
  module InputMethod = InputMethod.Impl(Editor);
  module Request = Request.Impl(Editor);
  open Belt;
  type editor = Editor.editor;
  type context = Editor.context;
  type t = {
    editor,
    context,
    view: Editor.view,
    mutable connection: option(Connection.t),
    mutable goals: array(Goal.t),
    mutable decorations: array(Editor.Decoration.t),
    mutable cursor: option(int),
    onDestroyEventEmitter: Event.t(unit),
    inputMethod: InputMethod.t,
    // onInputMethodAction: Event.t(Command.InputMethodAction.t),
    // mutable inputMethodActivated: bool,
    // onViewInquiryResponse: Event.t(option(string)),
  };

  //
  // getters
  //
  let getEditor = (state: t) => state.editor;

  //
  // events
  //
  let onceDestroyed = state => state.onDestroyEventEmitter.once();

  //
  // Agda connection/disconnection
  //

  // connect if not connected yet
  let connect = state =>
    switch (state.connection) {
    | None =>
      Connection.make(Editor.Config.getAgdaPath, Editor.Config.setAgdaPath)
      ->Promise.mapError(e => Error.Connection(e))
      ->Promise.tapOk(conn => state.connection = Some(conn))
    | Some(connection) => Promise.resolved(Ok(connection))
    };
  let disconnect = state =>
    switch (state.connection) {
    | None => Promise.resolved()
    | Some(connection) =>
      Connection.destroy(connection);
      Promise.resolved();
    };

  //
  // construction/destruction
  //

  // set context so that only certain key bindings only work
  // when there's a active text editor
  let setLoaded = value => Editor.setContext("agdaMode", value)->ignore;

  let destroy = state => {
    state.view->Editor.View.destroy;
    state.onDestroyEventEmitter.emit();
    state.onDestroyEventEmitter.destroy();
    state.goals->Array.forEach(Goal.destroy);
    state.decorations->Array.forEach(Editor.Decoration.destroy);
    setLoaded(false);
    state->disconnect;
  };

  let make = (context, editor) => {
    setLoaded(true);
    // view initialization
    let view = Editor.View.make(context, editor);

    let state = {
      editor,
      context,
      view,
      connection: None,
      goals: [||],
      decorations: [||],
      cursor: None,
      onDestroyEventEmitter: Event.make(),
      inputMethod: InputMethod.make(),
    };

    state;
  };

  //
  // View-related
  //

  let show = state => {
    state.view->Editor.View.show;
    setLoaded(true);
  };
  let hide = state => {
    state.view->Editor.View.hide;
    setLoaded(false);
  };
  let sendRequestToView = (state, request) =>
    Editor.View.send(state.view, View.RequestOrEventToView.Request(request));
  let sendEventToView = (state, event) =>
    Editor.View.send(state.view, View.RequestOrEventToView.Event(event));
};
