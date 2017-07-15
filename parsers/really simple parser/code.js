function CONS(a,b) { return {HEAD:a,TAIL: b}; };

// classes
function Letter(c)  { this.val = c; }
function Integer(c) { this.val = c; }
function Op(c)      { this.val = c; }
 Letter.prototype.toString = function LetterToString()  { return "Letter("+this.val+")"; }
Integer.prototype.toString = function IntegerToString() { return "Integer("+this.val+")"; }
     Op.prototype.toString = function OpToString()      { return "Op("+this.val+")"; }
// yes, classes in javascript are weird
// more classes
function Tree(value, branches) {
    this.val = value;
    this.branches = branches; // this muss be an array
}
Tree.prototype.toString = function TreeToString() { return "Tree("+this.val+", ["+this.branches+"])"; }
//

function lexer(s) {
    var ts = s.replace(/\s+$/g,'').split(/\s+/); // spliting by blank characters
    var tokens = new Array();

    // detecting tokens
    for(i=0;i<ts.length;i++) {
        switch (true) {
            case /^[a-z]$/    .test(ts[i]): tokens.push(new Letter(ts[i]));  break;
            case /^[0-9]+$/   .test(ts[i]): tokens.push(new Integer(ts[i])); break;
            case /^[+*\-\/]$/ .test(ts[i]): tokens.push(new Op(ts[i]));      break;
            default: return ts[i];
        }
    }
    tokens.push("EOF");
    return tokens;
}

/* Grammar:
 * <Expr>    --> <Op><Expr><Expr> | <Primary>
 * <Op>      --> + | - | * | /
 * <Primary> --> <Integer> | <Letter>
 *
 * Grammar simplified
 * <Expr> --> <Op><Expr><Expr> | <Integer> | <Letter>
 */
function parser(tokens) {
    // this is the 'Grammar simplified', see three lines up
    function expr(ts) {
        // is the token array empty O_O, WTF!
        if (ts.length==0) alert("error parsing: EOF not founded");
        // head (a token) of the buffer (token array)
        head = ts[0];
        switch (true) {
            // <Expr> --> <Op><Expr><Expr>
            case head instanceof Op:
                // deleting the head of the buffer, consuming
                ts.shift();
                // returning a three with three branches:
                // the value of the Op, and two more Expr
                return new Tree("Expr", [head, expr(ts), expr(ts)]);
            // if the token is an Integer or a Letter, then just return it
            // <Expr> --> <Integer>
            case head instanceof Integer:
            // <Expr> --> <Letter>
            case head instanceof Letter:
                // deleting the head of the buffer, consuming
                ts.shift();
                return head;
            default:
                // the token array have an unexpected token, WTF!
                alert("error parsing: '"+head+"'");
                return null;
        }
    }

    // copying the array
    var ts = tokens.slice(0);
    // parsing array, at the end muss be only one token in the array 'ts' (ts == ["EOF"])
    var tree = expr(ts);

    // errors of parsing
    if (ts.length != 1) alert("error parsing: end of parsing without reach EOF\nleftover tokens: ["+ts+"]");
    if (ts.length == 0) alert("error parsing: EOF not founded");

    // returning the parsing tree
    return tree;
}
