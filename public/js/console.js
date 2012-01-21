//Debugging methods - log information to console (if available)
if (!window.console) {
    console = {};
}

console.log = console.log || function () { };
console.warn = console.warn || function () { };
console.error = console.error || function () { };
console.info = console.info || function() { };

define(null);

