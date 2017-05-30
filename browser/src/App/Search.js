"use strict";

var lunr = require('lunr');

exports._loadIndex = function (data) {
  var parsed = JSON.parse(data);
  console.log(parsed);
  return lunr.Index.load(parsed);
};

exports._searchIn = function (idx) {
  return function (query) {
    return idx.search(query);
  };
};
