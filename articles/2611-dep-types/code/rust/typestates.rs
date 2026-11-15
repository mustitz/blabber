struct Fresh;
struct Connected;
struct Closed;

struct Socket<S> {
    state: S,
}

impl<S> Socket<S> {
    fn state_name(&self) -> &'static str {
        std::any::type_name_of_val(&self.state)
    }
}

fn socket() -> Socket<Fresh> {
    println!("socket()");
    Socket { state: Fresh }
}

impl Socket<Fresh> {
    fn set_option(&mut self, name: &str) {
        println!("setsockopt({name}) state={}", self.state_name());
    }

    fn connect(self) -> Socket<Connected> {
        println!("connect() state={}", self.state_name());
        Socket { state: Connected }
    }
}

trait Open {}
impl Open for Fresh {}
impl Open for Connected {}

impl<S: Open> Socket<S> {
    fn close(self) -> Socket<Closed> {
        println!("close() state={}", self.state_name());
        Socket { state: Closed }
    }
}

fn main() {
    let mut s = socket();
    s.set_option("SO_RCVBUF");
    s.set_option("SO_SNDBUF");

    let s = s.connect();
    // s.set_option("SO_RCVBUF");  // compiler error
    // s.connect();                // compiler error

    let s = s.close();
    println!("final state={}", s.state_name());
    // s.close();                  // compiler error
}
