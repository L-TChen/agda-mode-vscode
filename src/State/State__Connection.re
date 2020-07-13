module Js' = Js;
open! Promise;
module Js = Js';

module Impl = (Editor: Sig.Editor) => {
  module State = State.Impl(Editor);
  module Request = Request.Impl(Editor);
  module ErroM = State__Error.Impl(Editor);
  module RespM = State__Response.Impl(Editor);
  open Belt;

  let sendRequestToAgda =
      (state, request): Promise.t(result(Connection.t, Error.t)) => {
    state
    ->State.connect
    ->Promise.mapOk(connection => {
        let version = connection.metadata.version;
        let filepath =
          Editor.getFileName(state.editor)->Option.getWithDefault("");
        let libraryPath = Editor.Config.getLibraryPath();
        let highlightingMethod = Editor.Config.getHighlightingMethod();
        let backend = Editor.Config.getBackend();
        let encoded =
          Request.encode(
            state.editor,
            version,
            filepath,
            backend,
            libraryPath,
            highlightingMethod,
            request,
          );
        Js.log2("<<<", encoded);
        Connection.send(encoded, connection);
        connection;
      });
  };

  let rec sendRequest = (dispatchCommand, request: Request.t, state: State.t) => {
    let lastResponses = [||];

    // this promise get resolved after the request to Agda is completed
    let (promise, resolve) = Promise.pending();
    let handle = ref(None);
    let handler: result(Connection.response, Connection.Error.t) => unit =
      fun
      | Error(error) => ErroM.handle(Error.Connection(error), state)->ignore
      | Ok(Parser.Incr.Event.Yield(Error(error))) =>
        ErroM.handle(Error.Parser(error), state)->ignore
      | Ok(Yield(Ok(NonLast(response)))) => {
          Js.log(">>> " ++ Response.toString(response));
          RespM.handle(
            dispatchCommand,
            sendRequest(dispatchCommand),
            response,
            state,
          )
          ->ignore;
        }
      | Ok(Yield(Ok(Last(priority, response)))) => {
          Js.log(
            ">>> "
            ++ string_of_int(priority)
            ++ " "
            ++ Response.toString(response),
          );
          Js.Array.push((priority, response), lastResponses)->ignore;
        }
      | Ok(Stop) => {
          Js.log(">>| ");
          resolve();
        };

    state
    ->sendRequestToAgda(request)
    ->Promise.flatMap(
        fun
        | Ok(connection) => {
            handle := Some(connection.Connection.emitter.on(handler));
            promise;
          }
        | Error(error) => {
            ErroM.handle(error, state)->flatMap(_ => promise);
          },
      )
    ->tap(() => (handle^)->Option.forEach(f => f())) // destructor
    ->map(() => {
        Js.Array.sortInPlaceWith(
          (x, y) => compare(fst(x), fst(y)),
          lastResponses,
        )
        ->Array.map(snd)
        ->Array.forEach(response => {
            // should handle response
            Js.log(Response.toString(response))
          });
        state;
      });
  };
};
