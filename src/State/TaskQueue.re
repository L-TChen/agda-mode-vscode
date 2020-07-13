type t = {
  queue: array(unit => Promise.t(unit)),
  // `busy` will be set to `true` if there are Tasks being executed
  // A semaphore to make sure that only one `kickStart` is running at a time
  mutable busy: bool,
};

let make = () => {queue: [||], busy: false};

// consumes Tasks in the `queues`
let rec kickStart = self =>
  if (!self.busy) {
    switch (Js.Array.shift(self.queue)) {
    | None => ()
    | Some(task) =>
      self.busy = true;
      task()
      ->Promise.get(() => {
          self.busy = false; // flip the semaphore back
          kickStart(self);
        });
    };
  };

// returns a Promise that resolves after the task has been executed
let add = (self, task: unit => Promise.t(unit)): Promise.t(unit) => {
  let (promise, resolve) = Promise.pending();

  let func = () => task()->Promise.tap(resolve);

  Js.Array.push(func, self.queue)->ignore;

  // kick start
  kickStart(self);

  promise;
};
