"use strict";

var lunr = require('lunr');

exports._loadIndex = function (data) {
  return lunr.Index.load(JSON.parse(data));
};

exports.searchIn = function (idx) {
  return function (query) {
    var results = idx.search(query);
    console.log(query, results);
    return results;
  };
};
