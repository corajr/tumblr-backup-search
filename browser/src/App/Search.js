"use strict";

var lunr = require('lunr');

exports._loadIndex = function (data) {
  var parsed = JSON.parse(data);
  console.log(parsed);
  return lunr.Index.load(parsed);
};

exports.searchIn = function (idx) {
  return function (query) {
    var results = idx.search(query);
    // console.log(query, results);
    return results.map(x => x.ref);
  };
};
