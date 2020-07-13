module Js' = Js;
open! Promise;
module Js = Js';

module Impl = (Editor: Sig.Editor) => {
  module State = State.Impl(Editor);
  module Request = Request.Impl(Editor);
  module ErroM = State__Error.Impl(Editor);
  module RespM = State__Response.Impl(Editor);
  open Belt;
  let rec sendAgdaRequest = (dispatchCommand, defer, state, req) => {
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
          defer(priority, response);
        }
      | Ok(Stop) => {
          Js.log(">>| ");
          resolve();
        };

    state
    ->State.sendRequestToAgda(req)
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
    ->Promise.tap(() => (handle^)->Option.forEach(f => f()));
  }
  and sendRequest = (dispatchCommand, request: Request.t, state: State.t) => {
    let lastResponses = [||];

    sendAgdaRequest(
      dispatchCommand,
      (priority, response) => {
        Js.Array.push((priority, response), lastResponses)->ignore
      },
      state,
      request,
    )
    ->map(_ => {
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
