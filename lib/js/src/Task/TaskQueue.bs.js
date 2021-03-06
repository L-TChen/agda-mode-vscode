// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var $$Promise = require("reason-promise/lib/js/src/js/promise.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Belt_Option = require("bs-platform/lib/js/belt_Option.js");
var Task$AgdaModeVscode = require("./Task.bs.js");
var Util$AgdaModeVscode = require("../Util/Util.bs.js");
var Caml_chrome_debugger = require("bs-platform/lib/js/caml_chrome_debugger.js");

function Impl(Editor) {
  var Task = Task$AgdaModeVscode.Impl(Editor);
  var getNextTask = function (self) {
    var match = self.view;
    var match$1 = self.agda;
    var match$2 = self.main;
    if (typeof match === "number") {
      if (typeof match$1 === "number") {
        if (match$2) {
          self.main = match$2[1];
          return match$2[0];
        } else {
          return ;
        }
      }
      if (match$1.tag) {
        var match$3 = match$1[1];
        var resolve = match$1[0];
        if (match$3) {
          self.agda = /* Closing */Caml_chrome_debugger.variant("Closing", 1, [
              resolve,
              match$3[1]
            ]);
          return match$3[0];
        } else {
          self.agda = /* Free */0;
          Curry._1(resolve, undefined);
          return ;
        }
      }
      var match$4 = match$1[0];
      if (match$4) {
        self.agda = /* Pending */Caml_chrome_debugger.variant("Pending", 0, [match$4[1]]);
        return match$4[0];
      } else {
        return ;
      }
    } else {
      if (match.tag) {
        var match$5 = match[1];
        var resolve$1 = match[0];
        if (match$5) {
          self.view = /* Closing */Caml_chrome_debugger.variant("Closing", 1, [
              resolve$1,
              match$5[1]
            ]);
          return match$5[0];
        } else {
          self.view = /* Free */0;
          Curry._1(resolve$1, undefined);
          return ;
        }
      }
      var match$6 = match[0];
      if (match$6) {
        self.view = /* Pending */Caml_chrome_debugger.variant("Pending", 0, [match$6[1]]);
        return match$6[0];
      } else {
        return ;
      }
    }
  };
  var make = function (execute) {
    return {
            main: /* [] */0,
            agda: /* Free */0,
            view: /* Free */0,
            execute: execute,
            busy: undefined,
            shouldResolveWhenEmptied: undefined
          };
  };
  var kickStart = function (self) {
    if (!Belt_Option.isNone(self.busy)) {
      return ;
    }
    var task = getNextTask(self);
    if (task !== undefined) {
      if (typeof task !== "number" && task.tag === /* AgdaRequest */1) {
        return $$Promise.get(Curry._2(self.execute, self, task), (function (keepRunning) {
                      if (keepRunning) {
                        return kickStart(self);
                      }
                      
                    }));
      }
      self.busy = task;
      return $$Promise.get(Curry._2(self.execute, self, task), (function (keepRunning) {
                    self.busy = undefined;
                    if (keepRunning) {
                      return kickStart(self);
                    }
                    
                  }));
    }
    var resolve = self.shouldResolveWhenEmptied;
    if (resolve !== undefined) {
      return Curry._1(resolve, undefined);
    }
    
  };
  var onEmptied = function (self) {
    if (!Belt_Option.isSome(self.busy)) {
      return $$Promise.resolved(undefined);
    }
    var match = $$Promise.pending(undefined);
    self.shouldResolveWhenEmptied = match[1];
    return match[0];
  };
  var forceDestroy = function (self) {
    self.main = /* [] */0;
    self.agda = /* Free */0;
    self.view = /* Free */0;
    return $$Promise.resolved(undefined);
  };
  var addToTheBack = function (self, tasks) {
    self.main = Belt_List.concat(self.main, tasks);
    return kickStart(self);
  };
  var addToTheFront = function (self, tasks) {
    var match = self.view;
    var match$1 = self.agda;
    if (typeof match === "number") {
      if (typeof match$1 === "number") {
        self.main = Belt_List.concat(tasks, self.main);
      } else if (match$1.tag) {
        self.agda = /* Closing */Caml_chrome_debugger.variant("Closing", 1, [
            match$1[0],
            Belt_List.concat(tasks, match$1[1])
          ]);
      } else {
        self.agda = /* Pending */Caml_chrome_debugger.variant("Pending", 0, [Belt_List.concat(tasks, match$1[0])]);
      }
    } else if (match.tag) {
      self.view = /* Closing */Caml_chrome_debugger.variant("Closing", 1, [
          match[0],
          Belt_List.concat(tasks, match[1])
        ]);
    } else {
      self.view = /* Pending */Caml_chrome_debugger.variant("Pending", 0, [Belt_List.concat(tasks, match[0])]);
    }
    return kickStart(self);
  };
  var toString = function (taskToString, param) {
    var main = param.main;
    var agda = param.agda;
    var view = param.view;
    var main$1 = "Main " + Util$AgdaModeVscode.Pretty.list(Belt_List.map(main, taskToString));
    var agda$1;
    agda$1 = typeof agda === "number" ? "" : (
        agda.tag ? "Agda# " + Util$AgdaModeVscode.Pretty.list(Belt_List.map(agda[1], taskToString)) : "Agda " + Util$AgdaModeVscode.Pretty.list(Belt_List.map(agda[0], taskToString))
      );
    var view$1;
    view$1 = typeof view === "number" ? "" : (
        view.tag ? "View# " + Util$AgdaModeVscode.Pretty.list(Belt_List.map(view[1], taskToString)) : "View " + Util$AgdaModeVscode.Pretty.list(Belt_List.map(view[0], taskToString))
      );
    return main$1 + ("\n" + (agda$1 + ("\n" + view$1)));
  };
  var isOccupied = function (self) {
    var match = self.agda;
    if (typeof match === "number") {
      return false;
    } else {
      return true;
    }
  };
  var close = function (self) {
    var remainingTasks = self.agda;
    if (typeof remainingTasks === "number") {
      return $$Promise.resolved(undefined);
    }
    if (remainingTasks.tag) {
      console.log("[ panic ] The Agda task queue has been released by someone else");
      return $$Promise.resolved(undefined);
    }
    var match = $$Promise.pending(undefined);
    self.agda = /* Closing */Caml_chrome_debugger.variant("Closing", 1, [
        match[1],
        remainingTasks[0]
      ]);
    kickStart(self);
    return match[0];
  };
  var addToTheBack$1 = function (self, tasks) {
    var agda = self.agda;
    if (typeof agda === "number") {
      self.agda = /* Pending */Caml_chrome_debugger.variant("Pending", 0, [tasks]);
      return kickStart(self);
    }
    if (agda.tag) {
      console.log("[ panic ] Cannot add task to the Agda task queue after it's been marked closed");
      return ;
    }
    self.agda = /* Pending */Caml_chrome_debugger.variant("Pending", 0, [Belt_List.concat(agda[0], tasks)]);
    return kickStart(self);
  };
  var Agda = {
    isOccupied: isOccupied,
    close: close,
    addToTheBack: addToTheBack$1
  };
  var isOccupied$1 = function (self) {
    var match = self.view;
    if (typeof match === "number") {
      return false;
    } else {
      return true;
    }
  };
  var close$1 = function (self) {
    var remainingTasks = self.view;
    if (typeof remainingTasks === "number") {
      return $$Promise.resolved(undefined);
    }
    if (remainingTasks.tag) {
      console.log("[ panic ] The View task queue has been released by someone else");
      return $$Promise.resolved(undefined);
    }
    var match = $$Promise.pending(undefined);
    self.view = /* Closing */Caml_chrome_debugger.variant("Closing", 1, [
        match[1],
        remainingTasks[0]
      ]);
    kickStart(self);
    return match[0];
  };
  var addToTheBack$2 = function (self, tasks) {
    var view = self.view;
    if (typeof view === "number") {
      self.view = /* Pending */Caml_chrome_debugger.variant("Pending", 0, [tasks]);
      return kickStart(self);
    }
    if (view.tag) {
      console.log("[ panic ] Cannot add task to the View task queue after it's been marked closed");
      return ;
    }
    self.view = /* Pending */Caml_chrome_debugger.variant("Pending", 0, [Belt_List.concat(view[0], tasks)]);
    return kickStart(self);
  };
  var View = {
    isOccupied: isOccupied$1,
    close: close$1,
    addToTheBack: addToTheBack$2
  };
  return {
          Task: Task,
          getNextTask: getNextTask,
          make: make,
          kickStart: kickStart,
          onEmptied: onEmptied,
          forceDestroy: forceDestroy,
          addToTheBack: addToTheBack,
          addToTheFront: addToTheFront,
          toString: toString,
          Agda: Agda,
          View: View
        };
}

exports.Impl = Impl;
/* Promise Not a pure module */
