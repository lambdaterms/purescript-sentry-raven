/* global exports */
"use strict";

// module Sentry

var Raven = require('raven');
// foreign import setContextImpl ∷ ∀ eff. EffFn2 (raven ∷ RAVEN | eff) Raven Foreign Unit


exports.setContextImpl = function(raven, ctx){
    raven.setContext(ctx);
};

exports.mergeContextImpl = function(raven, ctx){
    raven.mergeContext(ctx);
};

exports.getContextImpl = function(raven){
    return raven.getContext();
};

exports.ravenImpl = function(dsn) {
  return new Raven.Client(dsn);
};

exports.captureMessageImpl = function(raven, msg) {
  raven.captureMessage(msg);
};

// -> this is evil:
// exports.extendContextImpl = function(ctx){
//     return function(newCtx){
//         return function(){
//             return extend(ctx, newCtx);
//         };};};

exports.inContextImpl = function(raven, ctx, eff) {
    return raven.context(ctx, eff);
};

exports.throw = function() {
  throw new Error("Test error");
};
