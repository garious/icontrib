var YOINK=(function(){var console=window&&window.console||{log:function(){}};var defaultInterpreters={json:function(text){return JSON.parse(text)},js:function(text,require,callback){var f_str="(function (baseUrl, define, require) {"+text+"})";var f=eval(f_str);f(require.base,callback,require)}};if(window&&window.execScript){defaultInterpreters.json=function(text){var f_str="(function () { return "+text+";})";window.execScript("_iesucks = "+f_str);return _iesucks()};defaultInterpreters.js=function(text,require,callback){var f_str="(function (baseUrl, define, require) {"+text+"})";window.execScript("_iesucks = "+f_str);var f=_iesucks;f(require.base,callback,require)}}function clone(o1){var o2={};for(k in o1){o2[k]=o1[k]}return o2}function interpret(rsc,url,interpreter,interpreters,cache,callback){if(!interpreter){var ext=url.substring(url.lastIndexOf(".")+1,url.length).toLowerCase();interpreter=interpreters[ext]||function(x){return x}}if(interpreter.length===1){callback(interpreter(rsc))}else{var base=url.substring(0,url.lastIndexOf("/"));var require=mkGetResources(base,cache,interpreters);require.base=base;interpreter(rsc,require,callback)}}function getFile(path,callback){var req=new XMLHttpRequest();req.onreadystatechange=function(){if(req.readyState===4){callback(req.responseText)}};req.open("GET",path,true);req.send()}var plans={};function interpretFile(interpreters,cache,u,str){console.log("yoink: interpreting '"+u.path+"'");interpret(str,u.path,u.interpreter,interpreters,cache,function(rsc){cache[u.path]=rsc;var plan=plans[u.path];delete plans[u.path];plan(rsc)})}function getResource(interpreters,cache,url,onInterpreted){var p=url.path;var rsc=cache[p];if(rsc===undefined){var plan=plans[p];if(plan===undefined){plans[p]=function(rsc){onInterpreted(rsc)};getFile(p,function(str){interpretFile(interpreters,cache,url,str)})}else{plans[p]=function(rsc){plan(rsc);onInterpreted(rsc)}}}else{onInterpreted(rsc)}}function resolve(base,url){var p=url.path||url;var f=url.interpreter||null;if(base!==""&&p.charAt(0)!=="/"&&p.indexOf("://")===-1){p=base+"/"+p}p=p.replace(/[^\/]+[\/]\.\.[\/]/g,"");return{path:p,interpreter:f}}function mkGetResources(base,cache,interpreters){return function(urls,callback){var rscs=[];var cnt=0;var len=urls.length;function mkOnInterpreted(i){return function(rsc){rscs[i]=rsc;cnt++;if(cnt===len){callback.apply(null,rscs)}}}for(var i=0;i<len;i++){var u=resolve(base,urls[i]);getResource(interpreters,cache,u,mkOnInterpreted(i))}}}function resourceLoader(base,cache,interpreters){base=base||"";cache=cache||{};interpreters=interpreters||clone(defaultInterpreters);return{getResources:mkGetResources(base,cache,interpreters)}}return{resourceLoader:resourceLoader,interpreters:defaultInterpreters}})();