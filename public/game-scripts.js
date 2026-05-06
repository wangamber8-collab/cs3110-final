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
