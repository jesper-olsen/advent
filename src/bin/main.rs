use adventure::Game;
use std::io::{self, Write};
use std::sync::mpsc;
use std::thread;

fn main() {
    // Channels for communication between terminal and game logic
    let (input_tx, input_rx) = mpsc::channel(); // input_rx -> Game's stdin
    let (output_tx, output_rx) = mpsc::channel(); // output_tx -> Game's stdout

    // Spawn a thread for the game logic
    let game_handle = thread::spawn(move || {
        let mut g = Game::new(input_rx, output_tx);
        g.game_loop();
    });

    // Spawn a thread for terminal input
    let input_handle = thread::spawn(move || {
        loop {
            let mut buffer = String::new();
            //print!("> "); // Terminal prompt
            io::stdout().flush().unwrap();
            if io::stdin().read_line(&mut buffer).is_err() {
                eprintln!("Failed to read input.");
                break;
            }
            let trimmed = buffer.trim();
            if input_tx.send(trimmed.to_string()).is_err() {
                //eprintln!("Game logic thread has ended.");
                break;
            }
        }
    });

    // Main thread handles output
    loop {
        match output_rx.recv() {
            Ok(message) => {
                print!("{}", message); // Print game output
                io::stdout().flush().unwrap();
            }
            Err(_) => {
                //eprintln!("Game logic thread has ended.");
                break;
            }
        }
    }

    input_handle.join().unwrap();
    game_handle.join().unwrap();
}
