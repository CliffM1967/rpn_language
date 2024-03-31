#[derive(PartialEq, Debug)]
enum Command {
    Integer(i64),
    Primitive(String),
    String(String),
    Symbol(String),
}

#[derive(PartialEq, Debug, Clone)]
enum StackEntry {
    Integer(i64),
    String(String),
}

use std::collections::HashMap;

struct Env {
    map: HashMap<String, StackEntry>,
    parent: Option<Box<Env>>,
}

impl Env {
    fn new() -> Self {
        Self {
            map: HashMap::new(),parent:None
        }
    }

    fn is_top(&self)->bool{
        self.parent.is_none()
    }

    fn define(&mut self, key: String, value: StackEntry) {
        self.map.insert(key, value);
    }

    fn lookup(&self, key: String) -> Option<StackEntry> {
        match self.map.get(&key) {
            Some(s) => Some(s.clone()), // found locally, return it.
            None => { 
                if self.is_top() { 
                    None // No parent, so return None
                } else {
                    // otherwise recursively call lookup on parent
                    self.parent.as_ref().unwrap().lookup(key)
                }
            },
        }
    }

    fn inner(self)->Self{
        let mut ie = Self::new();
        ie.parent = Some(Box::new(self));
        ie
    }

    fn outer(self)->Option<Self>{
        if self.is_top(){
            return None
        }
        Some(*self.parent.unwrap())
    }
}

fn parse(prog: &str) -> Vec<Command> {
    let mut ret = Vec::<Command>::new();
    for token in tokenise(prog) {
        // if token starts with lowercase char then it's a Symbol
        if token.chars().nth(0).unwrap().is_lowercase(){
            ret.push(Command::Symbol(token.to_string()));
            continue;
        }
        // are we an Integer ?
        let i = token.parse::<i64>();
        if i.is_ok() {
            ret.push(Command::Integer(i.unwrap()));
            continue;
        }
        // are we a String ?
        if token.chars().nth(0).unwrap() == '[' {
            // strip leading and trailing brackets
            let r = token[1..token.len() - 1].to_string();
            ret.push(Command::String(r));
            continue;
        }
        // if all the above fail, then we are a Primitive.
        ret.push(Command::Primitive(token.to_string()))
    }
    // we are going to be popping things off so get order right
    ret.reverse();
    ret
}

fn pop_int(stack: &mut Vec<StackEntry>) -> i64 {
    match stack.pop().unwrap() {
        // fetch top of stack and match it
        StackEntry::Integer(i) => i,
        o => panic!("Cannot get integer from {:?}", o),
    }
}

fn run_define(env: &mut Env, stack: &mut Vec<StackEntry>) {
    let key = pop_string(stack);
    let value = stack.pop().expect("Cannot pop stack");
    env.define(key, value);
}

// process this command which takes 2 ints and pushes an int back on stack
fn run_bin_int_op(cmd: &str, mut stack: &mut Vec<StackEntry>) {
    let i1 = pop_int(&mut stack);
    let i2 = pop_int(&mut stack);
    let result = match cmd {
        "+" => i1 + i2,
        "*" => i1 * i2,
        o => panic!("{}", format!("Binary op {o} not defined")),
    };
    stack.push(StackEntry::Integer(result));
}

fn pop_string(stack: &mut Vec<StackEntry>) -> String {
    let tos = stack.pop().expect("Cannot pop stack -- it's empty");
    match tos {
        StackEntry::String(s) => s,
        o => panic!("{}", format!("Expected StackEntry::String not {:?}", o)),
    }
}

// replace TOS with its looked up value
fn run_lookup(env: &mut Env, stack: &mut Vec<StackEntry>) {
    let key: String = pop_string(stack);
    let value = env.lookup(key).unwrap();
    stack.push(value);
}

fn run_execute(cmd_stack:&mut Vec<Command>,stack :&mut Vec<StackEntry>){
    // get the string to execute from the stack
    let s = pop_string(stack);
    let cmds = parse(&s);
    cmd_stack.extend(cmds);
}


fn run_env(prog: &str) -> (Vec<StackEntry>, Env) {
    let mut stack = Vec::<StackEntry>::new();
    let mut env = Env::new();
    let mut cmd_stack = parse(prog);

    loop {
        let command = cmd_stack.pop();
        // if no commands left, time to return 
        if command==None { return (stack,env) }
        let command = command.unwrap();     
        match command {
            Command::Integer(i) => stack.push(StackEntry::Integer(i)),
            Command::Primitive(p) => {
                let tm: &str = &p; // force conversion to &str
                match tm {
                    "DEFINE" => run_define(&mut env, &mut stack),
                    "LOOKUP" => run_lookup(&mut env, &mut stack),
                    "EXECUTE"=> run_execute(&mut cmd_stack,&mut stack),
                    "+" | "*" => run_bin_int_op(tm, &mut stack),
                    o => panic!("{}", format!("{o} not defined")),
                }
            }
            Command::String(s) => stack.push(StackEntry::String(s)),
            Command::Symbol(s) => {
                // lookup definition for symbol
                // TODO : make this recursive to find root definition
                let def = env.lookup(s).unwrap();
                stack.push(def);
                // push EXECUTE onto cmd_stack which will execute it
                cmd_stack.push(Command::Primitive("EXECUTE".to_string()));
            },
            _ => todo!(),
        }
    }
}

// split up this input string into tokens -- answer a Vector of Strings
fn tokenise(s: &str) -> Vec<String> {
    let mut vs = Vec::<String>::new(); // this is what we will return
    let mut ci = s.chars(); // get an interator over our input string
    let mut token = String::new();  // this will hold each token we find
    'outer: loop {
        let nc = ci.next();
        // if we have reached end of chars, return tokens so far
        if nc == None {
            return vs;
        }
        let c = nc.unwrap();
        // ignore whitespace 
        if c.is_whitespace() {
            continue;
        }
        if c == '[' {
            // start of sub-string, so gather it up
            token = c.to_string();
            loop {
                let nc = ci.next();
                // panic if we reach end without closing ']'
                if nc == None {
                    panic!("Cannot find closing ']' in {s}")
                }
                let c = nc.unwrap();
                token.push(c);
                if c == ']' {
                    vs.push(token);
                    continue 'outer; // start main loop again
                }
            }
        }
        // start collecting token from here
        token = c.to_string();
        if c.is_alphanumeric(){
            loop {
                let nc = ci.next();
                // if we have reached end, push on latest token and return
                if nc == None {
                    vs.push(token);
                    return vs;
                }
                let c = nc.unwrap();
                // if we hit whitespace, end of token
                if c.is_whitespace() {
                    vs.push(token);
                    continue 'outer; // continue main loop
                }
                // gather the next char in our token
                if !c.is_alphanumeric(){
                    // if not alphanumeric, panic.
                    panic!("Bad tokenisation {token} {c} insert a space");
                }
                token.push(c);
            }
        } // initial char was not alphanumeric so it's a single char token
        // note that it is not '[' either
        vs.push(token);
        continue 'outer;
    }
}

// answer just the stack -- to stop tests breaking
fn run(prog: &str) -> Vec<StackEntry> {
    let (stack, _) = run_env(prog);
    stack
}

#[test]
fn tokenise_empty_string() {
    assert_eq!(tokenise(""), Vec::<String>::new());
}

#[test]
fn tokenise_pure_whitespace() {
    assert_eq!(tokenise("   "), Vec::<String>::new());
}

#[test]
fn tokenise_single_token() {
    assert_eq!(tokenise("a"), vec!["a".to_string()]);
    assert_eq!(tokenise(" a "), vec!["a".to_string()]);
}

#[test]
fn tokenise_multiple_tokens() {
    assert_eq!(
        tokenise(
            " ab bcd"
        ),
        vec!["ab".to_string(), "bcd".to_string()]
    );
}

#[test]
fn tokenise_empty_substring() {
    assert_eq!(tokenise("[]"), vec!["[]".to_string()])
}

#[test]
fn tokenise_non_null_substring() {
    assert_eq!(tokenise("[ a ]"), vec!["[ a ]".to_string()])
}

#[test]
fn test_null() {
    let prog = "";
    let result = Vec::<StackEntry>::new();
    assert_eq!(run(prog), result)
}

#[test]
fn test_one() {
    let result = vec![StackEntry::Integer(1)];
    assert_eq!(run("1"), result);
}

#[test]
fn test_two() {
    let result = vec![StackEntry::Integer(2)];
    assert_eq!(run("2"), result);
}

#[test]
fn test_two_integers() {
    let result = vec![StackEntry::Integer(1), StackEntry::Integer(2)];
    assert_eq!(run("1 2"), result)
}

#[test]
fn test_add() {
    let result = vec![StackEntry::Integer(3)];
    assert_eq!(run("1 2 +"), result)
}

#[test]
fn test_parse_empty_string() {
    let cs = Vec::<Command>::new();
    assert_eq!(parse(""), cs)
}

#[test]
fn test_parse_single_token() {
    let cs: Vec<Command> = vec![Command::Integer(1)];
    assert_eq!(parse("1"), cs);
}

#[test]
fn test_complex_run() {
    let input = "1 2 + 3 +";
    let result = run(input);
    assert_eq!(result, vec![StackEntry::Integer(6)]);
}

#[test]
fn test_multiply() {
    assert_eq!(run("2 3 *"), vec![StackEntry::Integer(6)]);
}

#[test]
fn test_parse_string() {
    assert_eq!(parse("[]"), vec![Command::String("".to_string())]);
}

#[test]
fn test_parse_string2() {
    assert_eq!(
        parse("[1 +]"),vec![Command::String("1 +".to_string())]);
}

#[test]
fn test_lookup() {
    let prog = "[1 +] [plus1] DEFINE 2 [plus1] LOOKUP";
    let mut stack = run(prog);
    println!("Stack is {:?}", stack);
    let tos = pop_string(&mut stack);
    assert_eq!(tos, "1 +");
}

#[test]
fn test_define_new() {
    let e = Env::new();
    // nothing in there so looking something up should return None
    let r = e.lookup("foo".to_string());
    assert_eq!(r, None);
}

#[test]
//#[ignore]
fn test_define_item() {
    let mut e = Env::new();
    e.define("foo".to_string(), StackEntry::Integer(1));
    let r = e.lookup("foo".to_string());
    assert_eq!(r, Some(StackEntry::Integer(1)));
}

#[test]
fn test_define_env() {
    let (_, e) = run_env("1 [a] DEFINE");
    assert_eq!(e.lookup("a".to_string()), Some(StackEntry::Integer(1)));
}

#[test]
fn test_lookup_env() {
    let prog = "1 [a] DEFINE [a] LOOKUP";
    let r = run(prog).pop().unwrap(); // grab TOS
    assert_eq!(r, StackEntry::Integer(1));
}

#[test]
fn test_execute(){
    let prog = "2 [1 +]EXECUTE";
    assert_eq!(run(prog),vec![StackEntry::Integer(3)]);
}

#[test]
fn test_tokenise_bug(){
    let r = tokenise("2 [1 +]");
    assert_eq!(r[0],"2");
    assert_eq!(r[1],"[1 +]");
}

#[test]
#[should_panic(expected="insert a space")]
fn test_tokenise_bug3(){
    tokenise("2[]");  // no space after 2, should panic
}

#[test]
fn test_tokenise_bug2(){
    let r = tokenise("::");
    assert_eq!(r[0],":");
    assert_eq!(r[1],":");
}

#[test]
fn test_parse_symbol(){
    let cmds = parse("foo");
    match &cmds[0]{
        Command::Symbol(s) => assert_eq!(s,"foo"),
        o => assert!(false),
    }
}

#[test]
fn test_auto_execute(){
    let prog = "[1 2][a]DEFINE a";
    let r = run(prog);
    assert_eq!(r,vec![StackEntry::Integer(1),StackEntry::Integer(2)]);
}

#[test]
fn test_recursive_define(){
    let prog = "[1 +][plus1]DEFINE [plus1][a]DEFINE [a][b]DEFINE 2 b";
    let r = run(prog);
    assert_eq!(r,vec![StackEntry::Integer(3)]);
}

#[test]
fn test_new_env_is_top(){
    let e = Env::new();
    assert!(e.is_top());
}

/*
#[test]
#[ignore]
fn test_inner_env(){
    let e = Env::new();
    e.define("a",StackEntry::Integer(1));
    let ie = e.inner();
    // check ie is not top
    assert!(!ie.is_top());
    // initially looking up will give us the outer definition
    assert_eq!(ie.lookup("a").unwrap(),StackEntry::Integer(1));
    ie.define("a",StackEntry::Integer(2));
    // check we have the new definition
    assert_eq!(ie.lookup("a").unwrap(),StackEntry::Integer(2));
    // recover outer Env
    let e = ie.outer();
    // check original value for a is 1 again
    assert_eq!(e.lookup("a").unwrap(),StackEntry::Integer(1));
    // we are at the very outer level
    assert!(e.is_top());
}
*/

#[test]
fn test_inner_env1(){
    let e = Env::new();
    let ie = e.inner();
    assert!(!ie.is_top());
}

#[test]
fn test_inner_env2(){
    let mut e = Env::new();
    e.define("a".to_string(),StackEntry::Integer(1));
    let mut ie = e.inner();
    // check we can see original definition
    assert_eq!(ie.lookup("a".to_string()).unwrap(),StackEntry::Integer(1));
    // redefine "a" locally
    ie.define("a".to_string(),StackEntry::Integer(2));
    // check we see it
    assert_eq!(ie.lookup("a".to_string()).unwrap(),StackEntry::Integer(2));
}


#[test]
fn test_outer_env(){
    let mut e = Env::new();
    e.define("a".to_string(),StackEntry::Integer(1));
    // move into an inner environment
    e = e.inner();
    // recover outer environment
    e = e.outer().unwrap();
    // check original definition now holds
    assert_eq!(e.lookup("a".to_string()).unwrap(),StackEntry::Integer(1));
    // check e is the top level
    assert!(e.is_top());
}

