module Js' = Js;
open! Promise;
module Js = Js';

module Impl = (Editor: Sig.Editor) => {
  module State = State.Impl(Editor);
  module Request = Request.Impl(Editor);
  module ErroM = State__Error.Impl(Editor);
  open Belt;
  let sendAgdaRequest = (defer, state, req) => {
    // this promise get resolved after the request to Agda is completed
    let (promise, resolve) = Promise.pending();
    let handle = ref(None);
    let handler: result(Connection.response, Connection.Error.t) => unit =
      fun
      | Error(error) => ErroM.handle(Error.Connection(error), state)->ignore
      | Ok(Parser.Incr.Event.Yield(Error(error))) =>
        ErroM.handle(Error.Parser(error), state)->ignore
      | Ok(Yield(Ok(NonLast(response)))) => {
          Js.log(
            ">>> " ++ Response.toString(response),
            // let tasks = ResponseHandler.handle(response);
            // runTasks(tasks);
          );
        }
      | Ok(Yield(Ok(Last(priority, response)))) => {
          Js.log(
            ">>> "
            ++ string_of_int(priority)
            ++ " "
            ++ Response.toString(response),
          );
          // let tasks = ResponseHandler.handle(response);
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
  };

  let sendRequest = (request: Request.t, state: State.t) => {
    sendAgdaRequest((_, _) => (), state, request)->map(_ => state);
  };
};
