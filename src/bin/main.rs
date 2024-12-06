use adventure::Game;
use std::io::{self, Write};
use std::sync::mpsc;
use std::thread;

fn main() {
    // For TUI (terminal), the game reads reads stdin and prints to stdout
    // We do that right here - and communicate with the engine via channels
    let (input_tx, input_rx) = mpsc::channel(); // input_rx -> Game's stdin
    let (output_tx, output_rx) = mpsc::channel(); // output_tx -> Game's stdout

    // game logic
    let game_handle = thread::spawn(move || {
        let mut g = Game::new(input_rx, output_tx);
        g.game_loop();
    });

    // read stdin
    let input_handle = thread::spawn(move || {
        loop {
            let mut buffer = String::new();
            io::stdout().flush().unwrap();
            if io::stdin().read_line(&mut buffer).is_err() {
                eprintln!("Failed to read input.");
                break;
            }
            if input_tx.send(buffer.trim().to_string()).is_err() {
                //eprintln!("Game logic thread has ended.");
                break;
            }
        }
    });

    // print to stdout
    while let Ok(message) = output_rx.recv() {
        print!("{}", message);
        io::stdout().flush().unwrap();
    }

    input_handle.join().unwrap();
    game_handle.join().unwrap();
}
