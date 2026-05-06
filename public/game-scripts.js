const o = document.getElementById("out");
const ws = new WebSocket(`ws://${location.host}/ws`);
const guess = document.getElementById("guess");
const submit = document.getElementById("submit");

const bracket1 = document.getElementById("bracket1");

ws.onmessage = (event) => {
    const msg = event.data;
    console.log(msg);

    if (msg.startsWith("BRACKET|")) {
        bracket1.textContent = "[" + msg.slice("BRACKET|".length) + "]";
    }

    const msg = event.data;
    console.log(msg);

    if (msg.startsWith("BRACKET|")) {
        bracket1.textContent = "[" + msg.slice("BRACKET|".length) + "]";
        showFeedback("correct");
    }

    if (msg.startsWith("INCORRECT|")) {
        showFeedback("incorrect");
    }

    if (msg.startsWith("WIN|")) {
        console.log("WIN message received:", msg);
        showVictory({ puzzleName: "Bracket City" });
    }
};

submit.onclick = () => {
    ws.send(guess.value);
    guess.value = "";
};

guess.addEventListener("keypress", (e) => {
    if (e.key === "Enter") {
        ws.send(guess.value);
        guess.value = "";
    }
});

function showVictory({ puzzleName, timeTaken, accuracy, streak }) {
  document.getElementById('vc-puzzle-name').textContent = puzzleName;

  const overlay = document.getElementById('victory-overlay');
  overlay.style.display = 'flex';

  const card = document.getElementById('victory-card');
  card.style.animation = 'none';
  card.offsetHeight; // force reflow
  card.style.animation = '';
}

function closeVictory() {
  document.getElementById('victory-overlay').style.display = 'none';
}

function loadNextPuzzle() {
  closeVictory();
  // next puzzle logic here
}


function showFeedback(type) {
    const input = document.getElementById("guess");
    const submitBtn = document.getElementById("submit");
    const msg = document.getElementById("feedback-msg");

    input.classList.remove("feedback-correct", "feedback-incorrect");
    submitBtn.classList.remove("feedback-correct", "feedback-incorrect");
    void input.offsetWidth;

    if (type === "correct") {
        msg.textContent = "✓ correct!";
        msg.style.color = "#639922";
    } else {
        msg.textContent = "✗ incorrect, try again";
        msg.style.color = "#E24B4A";
    }

    input.classList.add(`feedback-${type}`);
    submitBtn.classList.add(`feedback-${type}`);

    setTimeout(() => {
        input.classList.remove("feedback-correct", "feedback-incorrect");
        submitBtn.classList.remove("feedback-correct", "feedback-incorrect");
        msg.textContent = "";
    }, 1500);
}