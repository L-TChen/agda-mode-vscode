module Header = {
  type t =
    | Plain(string)
    | Success(string)
    | Warning(string)
    | Error(string);

  let toString =
    fun
    | Plain(string) => string
    | Success(string) => string
    | Warning(string) => string
    | Error(string) => string;

  open Json.Decode;
  open Util.Decode;

  let decode: decoder(t) =
    sum(
      fun
      | "Plain" => Contents(string |> map(text => Plain(text)))
      | "Success" => Contents(string |> map(text => Success(text)))
      | "Warning" => Contents(string |> map(text => Warning(text)))
      | "Error" => Contents(string |> map(text => Error(text)))
      | tag => raise(DecodeError("[Header] Unknown constructor: " ++ tag)),
    );

  open! Json.Encode;
  let encode: encoder(t) =
    fun
    | Plain(text) =>
      object_([("tag", string("Plain")), ("contents", text |> string)])
    | Success(text) =>
      object_([("tag", string("Success")), ("contents", text |> string)])
    | Warning(text) =>
      object_([("tag", string("Warning")), ("contents", text |> string)])
    | Error(text) =>
      object_([("tag", string("Error")), ("contents", text |> string)]);
};

module Prompt = {
  type t = {
    body: option(string),
    placeholder: option(string),
    value: option(string),
  };

  // JSON encode/decode

  open Json.Decode;
  open Util.Decode;

  let decode: decoder(t) =
    sum(
      fun
      | "Prompt" =>
        Contents(
          tuple3(optional(string), optional(string), optional(string))
          |> map(((body, placeholder, value)) => {body, placeholder, value}),
        )
      | tag => raise(DecodeError("[Prompt] Unknown constructor: " ++ tag)),
    );

  open! Json.Encode;
  let encode: encoder(t) =
    fun
    | {body, placeholder, value} =>
      object_([
        ("tag", string("Prompt")),
        (
          "contents",
          (body, placeholder, value)
          |> tuple3(nullable(string), nullable(string), nullable(string)),
        ),
      ]);
};
module Body = {
  module Emacs = {
    type t =
      | Outputs
      | AllGoalsWarnings
      | GoalType
      | SearchAbout
      | Error
      | Text;

    open Json.Decode;
    open Util.Decode;

    let decode: decoder(t) =
      sum(
        fun
        | "Outputs" => TagOnly(Outputs)
        | "AllGoalsWarnings" => TagOnly(AllGoalsWarnings)
        | "GoalType" => TagOnly(GoalType)
        | "SearchAbout" => TagOnly(SearchAbout)
        | "Error" => TagOnly(Error)
        | "Text" => TagOnly(Text)
        | tag =>
          raise(DecodeError("[Body.Emacs] Unknown constructor: " ++ tag)),
      );

    open! Json.Encode;
    let encode: encoder(t) =
      fun
      | Outputs => object_([("tag", string("Outputs"))])
      | AllGoalsWarnings => object_([("tag", string("AllGoalsWarnings"))])
      | GoalType => object_([("tag", string("GoalType"))])
      | SearchAbout => object_([("tag", string("SearchAbout"))])
      | Error => object_([("tag", string("Error"))])
      | Text => object_([("tag", string("Text"))]);
  };

  type t =
    | Nothing
    | Plain(string)
    | Emacs(Emacs.t, string, string);

  open Json.Decode;
  open Util.Decode;

  let decode: decoder(t) =
    sum(
      fun
      | "Nothing" => TagOnly(Nothing)
      | "Plain" => Contents(string |> map(text => Plain(text)))
      | "Emacs" =>
        Contents(
          tuple3(Emacs.decode, string, string)
          |> map(((kind, header, body)) => Emacs(kind, header, body)),
        )
      | tag => raise(DecodeError("[Body] Unknown constructor: " ++ tag)),
    );

  open! Json.Encode;
  let encode: encoder(t) =
    fun
    | Nothing => object_([("tag", string("Nothing"))])
    | Plain(text) =>
      object_([("tag", string("Plain")), ("contents", text |> string)])
    | Emacs(kind, header, body) =>
      object_([
        ("tag", string("Emacs")),
        (
          "contents",
          (kind, header, body) |> tuple3(Emacs.encode, string, string),
        ),
      ]);
};

open Belt;
module Position = {
  type t = {
    pos: option(int),
    line: int,
    col: int,
  };

  open Json.Decode;
  open Util.Decode;

  let decode: decoder(t) =
    sum(
      fun
      | "Position" =>
        Contents(
          tuple3(optional(int), int, int)
          |> map(((pos, line, col)) => {pos, line, col}),
        )
      | tag =>
        raise(DecodeError("[View.Position] Unknown constructor: " ++ tag)),
    );

  open! Json.Encode;
  let encode: encoder(t) =
    fun
    | {pos, line, col} =>
      object_([
        ("tag", string("Position")),
        ("contents", (pos, line, col) |> tuple3(nullable(int), int, int)),
      ]);
};

module Interval = {
  type t = {
    start: Position.t,
    end_: Position.t,
  };

  let fuse = (a, b) => {
    let start =
      if (a.start.pos > b.start.pos) {
        b.start;
      } else {
        a.start;
      };
    let end_ =
      if (a.end_.pos > b.end_.pos) {
        a.end_;
      } else {
        b.end_;
      };
    {start, end_};
  };

  let toString = (self): string =>
    if (self.start.line === self.end_.line) {
      string_of_int(self.start.line)
      ++ ","
      ++ string_of_int(self.start.col)
      ++ "-"
      ++ string_of_int(self.end_.col);
    } else {
      string_of_int(self.start.line)
      ++ ","
      ++ string_of_int(self.start.col)
      ++ "-"
      ++ string_of_int(self.end_.line)
      ++ ","
      ++ string_of_int(self.end_.col);
    };

  open Json.Decode;
  open Util.Decode;

  let decode: decoder(t) =
    sum(
      fun
      | "Interval" =>
        Contents(
          pair(Position.decode, Position.decode)
          |> map(((start, end_)) => {start, end_}),
        )
      | tag =>
        raise(DecodeError("[View.Interval] Unknown constructor: " ++ tag)),
    );

  open! Json.Encode;
  let encode: encoder(t) =
    fun
    | {start, end_} =>
      object_([
        ("tag", string("Interval")),
        (
          "contents",
          (start, end_) |> pair(Position.encode, Position.encode),
        ),
      ]);
};

module Range = {
  type t =
    | NoRange
    | Range(option(string), array(Interval.t));

  let parse =
    [%re
      /*          |  different row                    |    same row            | */
      "/^(\\S+)\\:(?:(\\d+)\\,(\\d+)\\-(\\d+)\\,(\\d+)|(\\d+)\\,(\\d+)\\-(\\d+))$/"
    ]
    ->Emacs__Parser.captures(captured => {
        open Option;
        let flatten = xs => xs->flatMap(x => x);
        let srcFile = captured[1]->flatten;
        let sameRow = captured[6]->flatten->isSome;
        if (sameRow) {
          captured[6]
          ->flatten
          ->flatMap(int_of_string_opt)
          ->flatMap(row =>
              captured[7]
              ->flatten
              ->flatMap(int_of_string_opt)
              ->flatMap(colStart =>
                  captured[8]
                  ->flatten
                  ->flatMap(int_of_string_opt)
                  ->flatMap(colEnd =>
                      Some(
                        Range(
                          srcFile,
                          [|
                            {
                              start: {
                                pos: None,
                                line: row,
                                col: colStart,
                              },
                              end_: {
                                pos: None,
                                line: row,
                                col: colEnd,
                              },
                            },
                          |],
                        ),
                      )
                    )
                )
            );
        } else {
          captured[2]
          ->flatten
          ->flatMap(int_of_string_opt)
          ->flatMap(rowStart =>
              captured[3]
              ->flatten
              ->flatMap(int_of_string_opt)
              ->flatMap(colStart =>
                  captured[4]
                  ->flatten
                  ->flatMap(int_of_string_opt)
                  ->flatMap(rowEnd =>
                      captured[5]
                      ->flatten
                      ->flatMap(int_of_string_opt)
                      ->flatMap(colEnd =>
                          Some(
                            Range(
                              srcFile,
                              [|
                                {
                                  start: {
                                    pos: None,
                                    line: rowStart,
                                    col: colStart,
                                  },
                                  end_: {
                                    pos: None,
                                    line: rowEnd,
                                    col: colEnd,
                                  },
                                },
                              |],
                            ),
                          )
                        )
                    )
                )
            );
        };
      });

  let fuse = (a: t, b: t): t => {
    open Interval;

    let mergeTouching = (l, e, s, r) =>
      List.concat(List.concat(l, [{start: e.start, end_: s.end_}]), r);

    let rec fuseSome = (s1, r1, s2, r2) => {
      let r1' = Util.List.dropWhile(x => x.end_.pos <= s2.end_.pos, r1);
      helpFuse(r1', [Interval.fuse(s1, s2), ...r2]);
    }
    and outputLeftPrefix = (s1, r1, s2, is2) => {
      let (r1', r1'') = Util.List.span(s => s.end_.pos < s2.start.pos, r1);
      List.concat(List.concat([s1], r1'), helpFuse(r1'', is2));
    }
    and helpFuse = (a: List.t(Interval.t), b: List.t(Interval.t)) =>
      switch (a, List.reverse(a), b, List.reverse(b)) {
      | ([], _, _, _) => a
      | (_, _, [], _) => b
      | ([s1, ...r1], [e1, ...l1], [s2, ...r2], [e2, ...l2]) =>
        if (e1.end_.pos < s2.start.pos) {
          List.concat(a, b);
        } else if (e2.end_.pos < s1.start.pos) {
          List.concat(b, a);
        } else if (e1.end_.pos === s2.start.pos) {
          mergeTouching(l1, e1, s2, r2);
        } else if (e2.end_.pos === s1.start.pos) {
          mergeTouching(l2, e2, s1, r1);
        } else if (s1.end_.pos < s2.start.pos) {
          outputLeftPrefix(s1, r1, s2, b);
        } else if (s2.end_.pos < s1.start.pos) {
          outputLeftPrefix(s2, r2, s1, a);
        } else if (s1.end_.pos < s2.end_.pos) {
          fuseSome(s1, r1, s2, r2);
        } else {
          fuseSome(s2, r2, s1, r1);
        }
      | _ => failwith("something wrong with Range::fuse")
      };
    switch (a, b) {
    | (NoRange, r2) => r2
    | (r1, NoRange) => r1
    | (Range(f, r1), Range(_, r2)) =>
      Range(
        f,
        helpFuse(List.fromArray(r1), List.fromArray(r2))->List.toArray,
      )
    };
  };

  let toString = (self: t): string =>
    switch (self) {
    | NoRange => ""
    | Range(None, xs) =>
      switch (xs[0], xs[Array.length(xs) - 1]) {
      | (Some(first), Some(last)) =>
        Interval.toString({start: first.start, end_: last.end_})
      | _ => ""
      }

    | Range(Some(filepath), [||]) => filepath
    | Range(Some(filepath), xs) =>
      filepath
      ++ ":"
      ++ (
        switch (xs[0], xs[Array.length(xs) - 1]) {
        | (Some(first), Some(last)) =>
          Interval.toString({start: first.start, end_: last.end_})
        | _ => ""
        }
      )
    };

  open Json.Decode;
  open Util.Decode;

  let decode: decoder(t) =
    sum(
      fun
      | "Range" =>
        Contents(
          pair(optional(string), array(Interval.decode))
          |> map(((source, intervals)) => Range(source, intervals)),
        )
      | "NoRange" => TagOnly(NoRange)
      | tag =>
        raise(DecodeError("[View.Range] Unknown constructor: " ++ tag)),
    );

  open! Json.Encode;
  let encode: encoder(t) =
    fun
    | Range(source, intervals) =>
      object_([
        ("tag", string("Range")),
        (
          "contents",
          (source, intervals)
          |> pair(nullable(string), array(Interval.encode)),
        ),
      ])
    | NoRange => object_([("tag", string("NoRange"))]);
};

module Link = {
  type t =
    | ToRange(Range.t)
    | ToHole(int);

  open Json.Decode;
  open Util.Decode;

  let decode: decoder(t) =
    sum(
      fun
      | "ToRange" => Contents(Range.decode |> map(range => ToRange(range)))
      | "ToHole" => Contents(int |> map(index => ToHole(index)))
      | tag =>
        raise(DecodeError("[View.Link] Unknown constructor: " ++ tag)),
    );

  open! Json.Encode;
  let encode: encoder(t) =
    fun
    | ToRange(range) =>
      object_([
        ("tag", string("ToRange")),
        ("contents", range |> Range.encode),
      ])
    | ToHole(index) =>
      object_([("tag", string("ToHole")), ("contents", index |> int)]);
};

module EventToView = {
  module InputMethod = {
    type t =
      | Activate
      | Deactivate
      | Update(string, Translator.translation, int)
      | MoveUp
      | MoveRight
      | MoveDown
      | MoveLeft;

    open Json.Decode;
    open Util.Decode;

    let decode: decoder(t) =
      sum(
        fun
        | "Activate" => TagOnly(Activate)
        | "Deactivate" => TagOnly(Deactivate)
        | "Update" =>
          Contents(
            tuple3(string, Translator.decode, int)
            |> map(((sequence, translation, index)) =>
                 Update(sequence, translation, index)
               ),
          )
        | "MoveUp" => TagOnly(MoveUp)
        | "MoveRight" => TagOnly(MoveRight)
        | "MoveDown" => TagOnly(MoveDown)
        | "MoveLeft" => TagOnly(MoveLeft)
        | tag =>
          raise(
            DecodeError(
              "[EventToView.InputMethod] Unknown constructor: " ++ tag,
            ),
          ),
      );

    open! Json.Encode;
    let encode: encoder(t) =
      fun
      | Activate => object_([("tag", string("Activate"))])
      | Deactivate => object_([("tag", string("Deactivate"))])
      | Update(sequence, translation, index) =>
        object_([
          ("tag", string("Update")),
          (
            "contents",
            (sequence, translation, index)
            |> tuple3(string, Translator.encode, int),
          ),
        ])
      | MoveUp => object_([("tag", string("MoveUp"))])
      | MoveRight => object_([("tag", string("MoveRight"))])
      | MoveDown => object_([("tag", string("MoveDown"))])
      | MoveLeft => object_([("tag", string("MoveLeft"))]);
  };

  type t =
    | Display(Header.t, Body.t)
    | PromptInterrupt
    | PromptIMUpdate(string)
    | InputMethod(InputMethod.t);

  // JSON encode/decode

  open Json.Decode;
  open Util.Decode;

  let decode: decoder(t) =
    sum(
      fun
      | "Display" =>
        Contents(
          pair(Header.decode, Body.decode)
          |> map(((header, body)) => Display(header, body)),
        )
      | "PromptInterrupt" => TagOnly(PromptInterrupt)
      | "PromptIMUpdate" =>
        Contents(string |> map(text => PromptIMUpdate(text)))
      | "InputMethod" =>
        Contents(InputMethod.decode |> map(x => InputMethod(x)))
      | tag =>
        raise(DecodeError("[EventToView] Unknown constructor: " ++ tag)),
    );

  open! Json.Encode;
  let encode: encoder(t) =
    fun
    | Display(header, body) =>
      object_([
        ("tag", string("Display")),
        ("contents", (header, body) |> pair(Header.encode, Body.encode)),
      ])
    | PromptInterrupt => object_([("tag", string("PromptInterrupt"))])
    | PromptIMUpdate(text) =>
      object_([
        ("tag", string("PromptIMUpdate")),
        ("contents", text |> string),
      ])
    | InputMethod(payload) =>
      object_([
        ("tag", string("InputMethod")),
        ("contents", payload |> InputMethod.encode),
      ]);
};

module Request = {
  type t =
    | Prompt(Header.t, Prompt.t);

  // JSON encode/decode

  open Json.Decode;
  open Util.Decode;

  let decode: decoder(t) =
    sum(
      fun
      | "Prompt" =>
        Contents(
          pair(Header.decode, Prompt.decode)
          |> map(((header, prompt)) => Prompt(header, prompt)),
        )
      | tag => raise(DecodeError("[Request] Unknown constructor: " ++ tag)),
    );

  open! Json.Encode;
  let encode: encoder(t) =
    fun
    | Prompt(header, prompt) =>
      object_([
        ("tag", string("Prompt")),
        (
          "contents",
          (header, prompt) |> pair(Header.encode, Prompt.encode),
        ),
      ]);
};

module RequestOrEventToView = {
  type t =
    | Request(Request.t)
    | Event(EventToView.t);

  // JSON encode/decode

  open Json.Decode;
  open Util.Decode;

  let decode: decoder(t) =
    sum(
      fun
      | "Request" => Contents(Request.decode |> map(x => Request(x)))
      | "Event" => Contents(EventToView.decode |> map(x => Event(x)))
      | tag =>
        raise(
          DecodeError("[RequestOrEventToView] Unknown constructor: " ++ tag),
        ),
    );

  open! Json.Encode;
  let encode: encoder(t) =
    fun
    | Request(payload) =>
      object_([
        ("tag", string("Request")),
        ("contents", payload |> Request.encode),
      ])
    | Event(payload) =>
      object_([
        ("tag", string("Event")),
        ("contents", payload |> EventToView.encode),
      ]);
};

module Response = {
  type t =
    | PromptSuccess(string)
    | PromptInterrupted;

  open Json.Decode;
  open Util.Decode;

  let decode: decoder(t) =
    sum(
      fun
      | "PromptSuccess" =>
        Contents(string |> map(result => PromptSuccess(result)))
      | "PromptInterrupted" => TagOnly(PromptInterrupted)
      | tag => raise(DecodeError("[Response] Unknown constructor: " ++ tag)),
    );

  open! Json.Encode;
  let encode: encoder(t) =
    fun
    | PromptSuccess(result) =>
      object_([
        ("tag", string("PromptSuccess")),
        ("contents", result |> string),
      ])
    | PromptInterrupted => object_([("tag", string("PromptInterrupted"))]);
};

module EventFromView = {
  module InputMethod = {
    type t =
      | InsertChar(string)
      | ChooseSymbol(string);

    open Json.Decode;
    open Util.Decode;

    let decode: decoder(t) =
      sum(
        fun
        | "InsertChar" => Contents(string |> map(char => InsertChar(char)))
        | "ChooseSymbol" =>
          Contents(string |> map(char => ChooseSymbol(char)))
        | tag =>
          raise(
            DecodeError("[Event.InputMethod] Unknown constructor: " ++ tag),
          ),
      );

    open! Json.Encode;
    let encode: encoder(t) =
      fun
      | InsertChar(char) =>
        object_([
          ("tag", string("InsertChar")),
          ("contents", char |> string),
        ])
      | ChooseSymbol(symbol) =>
        object_([
          ("tag", string("ChooseSymbol")),
          ("contents", symbol |> string),
        ]);
  };

  type t =
    | Initialized
    | Destroyed
    | InputMethod(InputMethod.t)
    | PromptChange(string)
    | JumpToTarget(Link.t)
    | MouseOver(Link.t)
    | MouseOut(Link.t);

  open Json.Decode;
  open Util.Decode;

  let decode: decoder(t) =
    sum(
      fun
      | "Initialized" => TagOnly(Initialized)
      | "Destroyed" => TagOnly(Destroyed)
      | "InputMethod" =>
        Contents(InputMethod.decode |> map(action => InputMethod(action)))
      | "PromptChange" => Contents(string |> map(text => PromptChange(text)))
      | "JumpToTarget" =>
        Contents(Link.decode |> map(link => JumpToTarget(link)))
      | "MouseOver" => Contents(Link.decode |> map(link => MouseOver(link)))
      | "MouseOut" => Contents(Link.decode |> map(link => MouseOut(link)))
      | tag =>
        raise(
          DecodeError(
            "[Response.EventFromView] Unknown constructor: " ++ tag,
          ),
        ),
    );

  open! Json.Encode;
  let encode: encoder(t) =
    fun
    | Initialized => object_([("tag", string("Initialized"))])
    | Destroyed => object_([("tag", string("Destroyed"))])
    | InputMethod(action) =>
      object_([
        ("tag", string("InputMethod")),
        ("contents", action |> InputMethod.encode),
      ])
    | PromptChange(text) =>
      object_([
        ("tag", string("PromptChange")),
        ("contents", text |> string),
      ])
    | JumpToTarget(link) =>
      object_([
        ("tag", string("JumpToTarget")),
        ("contents", link |> Link.encode),
      ])
    | MouseOver(link) =>
      object_([
        ("tag", string("MouseOver")),
        ("contents", link |> Link.encode),
      ])
    | MouseOut(link) =>
      object_([
        ("tag", string("MouseOut")),
        ("contents", link |> Link.encode),
      ]);
};

module ResponseOrEventFromView = {
  type t =
    | Response(Response.t)
    | Event(EventFromView.t);

  // JSON encode/decode

  open Json.Decode;
  open Util.Decode;

  let decode: decoder(t) =
    sum(
      fun
      | "Response" => Contents(Response.decode |> map(x => Response(x)))
      | "Event" => Contents(EventFromView.decode |> map(x => Event(x)))
      | tag =>
        raise(
          DecodeError(
            "[ResponseOrEventFromView] Unknown constructor: " ++ tag,
          ),
        ),
    );

  open! Json.Encode;
  let encode: encoder(t) =
    fun
    | Response(payload) =>
      object_([
        ("tag", string("Response")),
        ("contents", payload |> Response.encode),
      ])
    | Event(payload) =>
      object_([
        ("tag", string("Event")),
        ("contents", payload |> EventFromView.encode),
      ]);
};
