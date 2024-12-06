use adventure::Game;
use std::io::{self, Write};
use std::sync::mpsc;
use std::thread;

fn print_and_flush(message: &str) -> io::Result<()> {
    print!("{}", message);
    io::stdout().flush()
}

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
        for line in io::stdin().lines() {
            match line {
                Ok(input) => {
                    if input_tx.send(input.trim().to_string()).is_err() {
                        eprintln!("Failed to send input to the game.");
                        break;
                    }
                }
                Err(e) => {
                    eprintln!("Error reading input: {e}");
                    break;
                }
            }
        }
    });

    for message in output_rx.iter() {
        if let Err(e) = print_and_flush(&message) {
            eprintln!("Error printing game output: {e}");
            break;
        }
    }

    if let Err(e) = input_handle.join() {
        eprintln!("Error in input thread: {:?}", e);
    }

    if let Err(e) = game_handle.join() {
        eprintln!("Error in game logic thread: {:?}", e);
    }
}
