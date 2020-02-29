dumpJSONResults = true;
function declareFFGlobals(obj) {
    obj.console = {
        log: print
    };
    obj.print = print;
    obj.read = read;
    obj.readFile = read;
    obj.runString = function(...args) { 
        if (args[0] === '') {
            let s = evalcx('');
            declareFFGlobals(s);
            return s;
        }
        return evalcx(...args);
    };
    obj.loadString = code => evalcx(code, obj);
    obj.preciseTime = dateNow;
}

let g = newGlobal();
declareFFGlobals(g);

let driver = 'cli.js';
evalcx(`
dumpJSONResults=true;
` + read(driver), g);

