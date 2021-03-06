// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Os = require("os");
var Diff = require("diff");
var Path = require("path");
var Util = require("util");
var Curry = require("bs-platform/lib/js/curry.js");
var $$Promise = require("reason-promise/lib/js/src/js/promise.js");
var Process = require("process");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Belt_Option = require("bs-platform/lib/js/belt_Option.js");
var Assert$BsMocha = require("bs-mocha/lib/js/src/Assert.bs.js");
var Caml_primitive = require("bs-platform/lib/js/caml_primitive.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");
var Promise$BsMocha = require("bs-mocha/lib/js/src/Promise.bs.js");
var Caml_js_exceptions = require("bs-platform/lib/js/caml_js_exceptions.js");
var Caml_chrome_debugger = require("bs-platform/lib/js/caml_chrome_debugger.js");

function Impl(Editor) {
  var Exn = Caml_exceptions.create("Test__Util-AgdaModeVscode.Impl(Editor).Exn");
  var toAbsolute = function (filepath) {
    var dirname = typeof __dirname === "undefined" ? undefined : __dirname;
    if (dirname !== undefined) {
      return Path.resolve(dirname, filepath);
    } else {
      return Process.cwd();
    }
  };
  var extensionPath = function (param) {
    var dirname = typeof __dirname === "undefined" ? undefined : __dirname;
    if (dirname !== undefined) {
      return Path.resolve(dirname, "../../../../");
    } else {
      return Process.cwd();
    }
  };
  var asset = function (filepath) {
    return Path.join(extensionPath(undefined), "test/tests/assets", filepath);
  };
  var Path$1 = {
    toAbsolute: toAbsolute,
    extensionPath: extensionPath,
    asset: asset
  };
  var wait = function (ms) {
    var match = $$Promise.pending(undefined);
    setTimeout(match[1], ms);
    return match[0];
  };
  var toPromise = function (f) {
    return new Promise((function (resolve, reject) {
                  return $$Promise.get(f, (function (error) {
                                if (error.tag) {
                                  return reject(error[0]);
                                } else {
                                  return resolve(error[0]);
                                }
                              }));
                }));
  };
  var it = function (s, f) {
    return Promise$BsMocha.it(s)(undefined, undefined, undefined, (function (param) {
                  return toPromise(Curry._1(f, undefined));
                }));
  };
  var it_only = function (s, f) {
    return Promise$BsMocha.it_only(s)(undefined, undefined, undefined, (function (param) {
                  return toPromise(Curry._1(f, undefined));
                }));
  };
  var it_skip = function (s, f) {
    return Promise$BsMocha.it_skip(s)(undefined, undefined, undefined, (function (param) {
                  return toPromise(Curry._1(f, undefined));
                }));
  };
  var before = function (f) {
    return Promise$BsMocha.before(undefined)(undefined, undefined, undefined, (function (param) {
                  return toPromise(Curry._1(f, undefined));
                }));
  };
  var before_each = function (f) {
    return Promise$BsMocha.before_each(undefined)(undefined, undefined, undefined, (function (param) {
                  return toPromise(Curry._1(f, undefined));
                }));
  };
  var after = function (f) {
    return Promise$BsMocha.after(undefined)(undefined, undefined, undefined, (function (param) {
                  return toPromise(Curry._1(f, undefined));
                }));
  };
  var after_each = function (f) {
    return Promise$BsMocha.after_each(undefined)(undefined, undefined, undefined, (function (param) {
                  return toPromise(Curry._1(f, undefined));
                }));
  };
  var Q = {
    toPromise: toPromise,
    it: it,
    it_only: it_only,
    it_skip: it_skip,
    before: before,
    before_each: before_each,
    after: after,
    after_each: after_each
  };
  var equal = function (expected, actual) {
    var tmp;
    try {
      Assert$BsMocha.equal(undefined, actual, expected);
      tmp = /* Ok */Caml_chrome_debugger.variant("Ok", 0, [undefined]);
    }
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      tmp = /* Error */Caml_chrome_debugger.variant("Error", 1, [exn]);
    }
    return $$Promise.resolved(tmp);
  };
  var A = {
    equal: equal
  };
  var normalize = function (string) {
    return string.trim().replace(/\r\n|\r/g, "\n");
  };
  var serialize = function (xs) {
    return xs.join("\n");
  };
  var serializeWith = function (f, xs) {
    return Belt_Array.map(xs, f).join("\n");
  };
  var breakInput = function (input, breakpoints) {
    var breakpoints$prime = Belt_Array.concat([0], breakpoints);
    return Belt_Array.map(Belt_Array.mapWithIndex(breakpoints$prime, (function (i, x) {
                      var next = Belt_Array.get(breakpoints$prime, i + 1 | 0);
                      if (next !== undefined) {
                        return /* tuple */[
                                x,
                                next - x | 0
                              ];
                      } else {
                        return /* tuple */[
                                x,
                                input.length - x | 0
                              ];
                      }
                    })), (function (param) {
                  return input.substr(param[0], param[1]);
                }));
  };
  var Strings = {
    normalize: normalize,
    serialize: serialize,
    serializeWith: serializeWith,
    breakInput: breakInput
  };
  var getValue = function (string) {
    return string[0];
  };
  var fromChangeObject = function (obj) {
    if (obj.added) {
      return /* Added */Caml_chrome_debugger.variant("Added", 0, [obj.value]);
    } else if (obj.removed) {
      return /* Removed */Caml_chrome_debugger.variant("Removed", 1, [obj.value]);
    } else {
      return /* NoChange */Caml_chrome_debugger.variant("NoChange", 2, [obj.value]);
    }
  };
  var wordsWithSpace = function (a, b) {
    return Belt_Array.map(Diff.diffWordsWithSpace(a, b), fromChangeObject);
  };
  var firstChange = function (diffs) {
    var count = {
      contents: 0
    };
    var change = {
      contents: undefined
    };
    Belt_Array.forEach(diffs, (function (diff) {
            if (!Belt_Option.isNone(change.contents)) {
              return ;
            }
            switch (diff.tag | 0) {
              case /* Added */0 :
                  change.contents = /* Added */Caml_chrome_debugger.variant("Added", 0, [diff[0]]);
                  return ;
              case /* Removed */1 :
                  change.contents = /* Removed */Caml_chrome_debugger.variant("Removed", 1, [diff[0]]);
                  return ;
              case /* NoChange */2 :
                  count.contents = count.contents + diff[0].length | 0;
                  return ;
              
            }
          }));
    return Belt_Option.map(change.contents, (function (change) {
                  return /* tuple */[
                          change,
                          count.contents
                        ];
                }));
  };
  var Diff$1 = {
    getValue: getValue,
    fromChangeObject: fromChangeObject,
    wordsWithSpace: wordsWithSpace,
    firstChange: firstChange
  };
  var getGoldenFilepaths = function (directoryPath) {
    var directoryPath$1 = toAbsolute(directoryPath);
    var readdir = Util.promisify((function (prim, prim$1) {
            Fs.readdir(prim, prim$1);
            
          }));
    var isInFile = function (param) {
      return param.endsWith(".in");
    };
    var toBasename = function (path) {
      return Path.join(directoryPath$1, Path.basename(path, ".in"));
    };
    return readdir(directoryPath$1).then((function (paths) {
                  return Promise.resolve(Belt_Array.map(Belt_Array.keep(paths, isInFile), toBasename));
                }));
  };
  var getGoldenFilepathsSync = function (directoryPath) {
    var directoryPath$1 = toAbsolute(directoryPath);
    var isInFile = function (param) {
      return param.endsWith(".in");
    };
    var toBasename = function (path) {
      return Path.join(directoryPath$1, Path.basename(path, ".in"));
    };
    return Belt_Array.map(Belt_Array.keep(Fs.readdirSync(directoryPath$1), isInFile), toBasename);
  };
  var FileMissing = Caml_exceptions.create("Test__Util-AgdaModeVscode.Impl(Editor).Golden.FileMissing");
  var map = function (param, f) {
    return /* Golden */Caml_chrome_debugger.simpleVariant("Golden", [
              param[0],
              Curry._1(f, param[1]),
              param[2]
            ]);
  };
  var readFile = function (filepath) {
    var filepath$1 = toAbsolute(filepath);
    var readFile$1 = Util.promisify((function (prim, prim$1) {
            Fs.readFile(prim, prim$1);
            
          }));
    return Promise.all([
                  readFile$1(filepath$1 + ".in"),
                  readFile$1(filepath$1 + ".out")
                ]).then((function (param) {
                  if (param.length !== 2) {
                    return Promise.reject([
                                FileMissing,
                                filepath$1
                              ]);
                  }
                  var input = param[0];
                  var output = param[1];
                  return Promise.resolve(/* Golden */Caml_chrome_debugger.simpleVariant("Golden", [
                                filepath$1,
                                input.toString(),
                                normalize(output.toString())
                              ]));
                }));
  };
  var compare = function (param) {
    var actual = normalize(param[1]);
    var expected = normalize(param[2]);
    Belt_Option.forEach(firstChange(wordsWithSpace(actual, expected)), (function (param) {
            var count = param[1];
            var diff = param[0];
            var value = diff[0];
            var expected$1 = expected.substr(Caml_primitive.caml_int_max(0, count - 50 | 0), (50 + value.length | 0) + 50 | 0);
            var actual$1 = actual.substr(Caml_primitive.caml_int_max(0, count - 50 | 0), (50 + value.length | 0) + 50 | 0);
            switch (diff.tag | 0) {
              case /* Added */0 :
              case /* Removed */1 :
                  return Assert$BsMocha.fail$prime(undefined, undefined, undefined, actual$1, expected$1);
              case /* NoChange */2 :
                  return ;
              
            }
          }));
    return Promise.resolve(undefined);
  };
  var Golden = {
    Diff: Diff$1,
    getGoldenFilepaths: getGoldenFilepaths,
    getGoldenFilepathsSync: getGoldenFilepathsSync,
    FileMissing: FileMissing,
    map: map,
    readFile: readFile,
    compare: compare
  };
  var onUnix = function (param) {
    var match = Os.type();
    if (match === "Windows_NT") {
      return false;
    } else {
      return true;
    }
  };
  return {
          Exn: Exn,
          Path: Path$1,
          wait: wait,
          Q: Q,
          A: A,
          Strings: Strings,
          Golden: Golden,
          onUnix: onUnix
        };
}

exports.Impl = Impl;
/* fs Not a pure module */
