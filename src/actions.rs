#[rustfmt::skip]
#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum Act {
    Abstain, Take, Drop, Open, Close, On, Off, Wave, Calm, Go, Relax,
    Pour, Eat, Drink, Rub, Toss, Wake, Feed, Fill, Break, Blast, Kill,
    Say, Read, Feefie, Brief, Find, Inventory, Score, Quit,
}

pub fn print_default_msg(action: Act) {
    if default_msg(action) != "" {
        println!("{}", default_msg(action));
    }
}

pub const fn default_msg(action: Act) -> &'static str {
    match action {
        Act::Abstain => "",
        Act::Blast => "Blasting requires dynamite.",
        Act::Brief => "On what?",
        Act::Break => "It is beyond your power to do that",
        Act::Calm => "I'm game. Would you care to explain how?",
        Act::Close => "I don't know how to lock or unlock such a thing.",
        Act::Drink => {
            "You have taken a drink from the stream. The water \
                  tastes strongly of minerals, but is not unpleasant. \
                  It is extremely cold."
        }
        Act::Eat | Act::Kill => "Don't be ridiculous!",
        Act::Feed => "There is nothing here to eat.",
        Act::Feefie => "I don’t know how.",
        Act::Fill => "You can't fill that.",
        Act::Find | Act::Inventory => {
            "I can only tell you what you see as you move \
                         about and manipulate things. I cannot tell you \
                         where remote things are."
        }
        Act::Go => "Where?",
        Act::On | Act::Off => "You have no source of light.",
        Act::Open => "I don't know how to lock or unlock such a thing.",
        Act::Pour | Act::Drop => "You aren't carrying it!",
        Act::Read => "I'm afraid I don't understand.",
        Act::Relax => "OK.",
        Act::Rub => {
            "Rubbing the electric lamp is not particularly rewarding. \
                Anyway, nothing exciting happens."
        }
        Act::Say => "",
        Act::Score | Act::Quit => "Eh?",
        Act::Take => "You are already carrying it!",
        Act::Toss => "Peculiar. Nothing unexpected happens.",
        Act::Wake => "Don't be ridiculous!",
        Act::Wave => "Nothing happens.",
    }
}
