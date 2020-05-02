module Impl = (Editor: Sig.Editor) => {
  open Belt;
  type editor = Editor.editor;
  type context = Editor.context;
  type t = {
    editor,
    context,
    view: Editor.view,
    mutable connection: option(Connection.t),
    onDestroyEventEmitter: Event.t(unit),
  };

  //
  // getters
  //
  let getEditor = (state: t) => state.editor;

  //
  // events
  //
  let onDestroy = (state, callback) => {
    state.onDestroyEventEmitter.on(callback)->Editor.Disposable.make;
  };

  //
  // GCL connection/disconnection
  //

  // connect if not connected yet
  let connect = state =>
    switch (state.connection) {
    | None =>
      Connection.make(Editor.Config.getAgdaPath, Editor.Config.setAgdaPath)
      ->Promise.mapError(e => Sig.Error.Connection(e))
      ->Promise.mapOk(conn => state.connection = Some(conn))
    | Some(_) => Promise.resolved(Ok())
    };
  let disconnect = state =>
    switch (state.connection) {
    | None => Promise.resolved()
    | Some(connection) => Connection.disconnect(connection)
    };

  let sendRequest = (state, request) => {
    let version = "2.6.1"; // TODO
    let filepath = "Editor.getFileName(state.editor)";
    let libraryPath = Editor.Config.getLibraryPath();
    let highlightingMethod = Editor.Config.getHighlightingMethod();
    let encoded =
      Request.encode(
        version,
        filepath,
        libraryPath,
        highlightingMethod,
        request,
      );
    Js.log2("<<<", encoded);
    // let%Ok conn = state->connect;
    // let%Ok result =
    //   Connection.send(encoded, conn)
    //   ->Promise.mapError(e => Sig.Error.Connection(e));
    // Js.log2(
    //   ">>>",
    //   Js.String.substring(~from=0, ~to_=200, Js.Json.stringify(result)),
    // );
    // catching exceptions occured when decoding JSON values
    // switch (Response.decode(result)) {
    // | value => Promise.resolved(Ok(value))
    // | exception (Json.Decode.DecodeError(msg)) =>
    //   Promise.resolved(Error(Sig.Error.Decode(msg, result)))
    // };
    // Promise.resolved(Ok());
    ();
  };

  //
  // construction/destruction
  //

  let destroy = state => {
    // state.view->Editor.View.destroy;
    state.onDestroyEventEmitter.destroy();
    state.onDestroyEventEmitter.emit();
    state->disconnect;
  };

  let make = (context, editor) => {
    // view initialization
    let view = Editor.View.make(context, editor);

    let state = {
      editor,
      context,
      view,
      connection: None,
      onDestroyEventEmitter: Event.make(),
    };

    // a dictionary of decorations for <Link>

    // update the state on receiving messages from the view
    // view
    // ->Editor.View.recv(onRecvMessageFromView)
    // ->Editor.addToSubscriptions(context);

    state;
  };
  //
  // View-related
  //
};