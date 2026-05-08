const o = document.getElementById("out");
const ws = new WebSocket(`ws://${location.host}/ws`);
const guess = document.getElementById("guess");
const submit = document.getElementById("submit");
const bracket1 = document.getElementById("bracket1");

ws.onopen = () => console.log("WebSocket connected");
ws.onerror = (e) => console.error("WebSocket error:", e);
ws.onclose = (e) => console.log("WebSocket closed:", e.code, e.reason);

ws.onmessage = (event) => {
    const msg = event.data;
    console.log(msg);

    if (msg.startsWith("BRACKET|")) {
        const text = msg.slice("BRACKET|".length);

        bracket1.innerHTML = text.replace(
        /\[([^\[\]]+)\]/g,
        '<span class="chip" data-hint="$1">[$1]</span>'
        );
        
        showFeedback("correct");
    }

    else if (msg.startsWith("PROGRESS|")) {
        const parts = msg.slice("PROGRESS|".length).split("|");
        const solved = parseInt(parts[0], 10);
        const total = parseInt(parts[1], 10);
        const pct = total > 0 ? (solved / total) * 100 : 0;
        document.getElementById("progress-bar").style.width = pct + "%";
        document.getElementById("progress-label").textContent =
            `${solved} of ${total} solved`;
    }

    else if (msg.startsWith("HINT|")) {
        const letter = msg.slice("HINT|".length);
        showHint(letter);
    }

    else if (msg.startsWith("INCORRECT|")) {
        showFeedback("incorrect");
    }

    else if (msg.startsWith("WIN|")) {
        console.log("WIN message received:", msg);
        
        bracket1.textContent = msg.slice("WIN|".length);

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

function showVictory({ puzzleName }) {
    document.getElementById('vc-puzzle-name').textContent = puzzleName;
    const overlay = document.getElementById('victory-overlay');
    overlay.style.display = 'flex';
    const card = document.getElementById('victory-card');
    card.style.animation = 'none';
    card.offsetHeight;
    card.style.animation = '';
    if (msg.startsWith("WIN|")) {
    console.log("WIN message received:", msg);  // do you see this?
    showVictory({ puzzleName: "Bracket City" });
    
}
}

function closeVictory() {
    document.getElementById('victory-overlay').style.display = 'none';
}

function loadNextPuzzle() {
    window.location.reload();
}

bracket1.addEventListener("click", (e) => {
    const chip = e.target.closest(".chip");
    if (chip) {
        ws.send("HINT|" + chip.dataset.hint);
    }
});

function showHint(letter) {
    const msg = document.getElementById("feedback-msg");
    msg.textContent = `hint: starts with "${letter}"`;
    msg.style.color = "#8096e7";
    setTimeout(() => {
        msg.textContent = "";
    }, 3000);
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