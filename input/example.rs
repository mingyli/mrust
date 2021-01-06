fn foo() -> i64 {
    let x: i64 = -5;
    let y: i64 = 9;
    x + y
}

fn main() -> () {
    let x: i64 = 7;
    let y: i64 = {
        let z: i64 = 3939;
        x + z
    };
    let z: i64 = foo() + 2 * foo();
}
