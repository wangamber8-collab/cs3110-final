const o = document.getElementById("out");
const diff = new URLSearchParams(location.search).get("difficulty") || "hard";
const ws = new WebSocket(`ws://${location.host}/ws?difficulty=${diff}`);
const guess = document.getElementById("guess");
const submit = document.getElementById("submit");
const bracket1 = document.getElementById("bracket1");
let latestStats = "";
let timerSeconds = 0;
let timerInterval = null;

function startTimer() {
    if (timerInterval !== null) return;

    timerInterval = setInterval(() => {
        timerSeconds += 1;
        document.getElementById("timer-label").textContent =
            formatTime(timerSeconds);
    }, 1000);
}

function formatTime(seconds) {
    const mins = Math.floor(seconds / 60);
    const secs = seconds % 60;

    if (mins === 0) {
        return `${secs}s`;
    }

    return `${mins}:${secs.toString().padStart(2, "0")}`;
}

ws.onopen = () => console.log("WebSocket connected");
ws.onerror = (e) => console.error("WebSocket error:", e);
ws.onclose = (e) => console.log("WebSocket closed:", e.code, e.reason);

ws.onmessage = (event) => {
    const msg = event.data;
    console.log(msg);

    if (msg.startsWith("BRACKET|")) {
    const text = msg.slice("BRACKET|".length);
    startTimer();

    bracket1.innerHTML = text.replace(
        /\[([^\[\]]+)\]/g,
        '<span class="chip" data-body="$1">[$1]</span>'
    );

    document.querySelectorAll(".chip").forEach((chip) => {
        chip.onclick = () => {
            const body = chip.dataset.body;

            if (chip.dataset.hinted === "true") {
                ws.send("REVEAL|" + body);
            } else {
                chip.dataset.hinted = "true";
                window.currentHintChip = chip;
                ws.send("HINT|" + body);
            }
        };
    });
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

    if (window.currentHintChip) {
        window.currentHintChip.innerHTML =
            window.currentHintChip.innerHTML + " (" + letter + ")";
    }
}

    else if (msg.startsWith("INCORRECT|")) {
        showFeedback("incorrect");
    }

    else if (msg.startsWith("TIMER|")) {
    const seconds = Number(msg.slice("TIMER|".length));

    document.getElementById("timer-label").textContent =
        formatTime(seconds);
}

else if (msg.startsWith("STATS|")) {
    latestStats = msg.slice("STATS|".length);
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
    clearInterval(timerInterval);
    
    document.getElementById('vc-puzzle-name').textContent = puzzleName;

    const stats = document.getElementById("vc-stats");
    if (stats && latestStats !== "") {
        const [total, wrong, hints, accuracy, score, , maxStreak] = latestStats.split("|");
        const correct = Number(total) - Number(wrong);

        stats.innerHTML =
            `<strong>Score: ${score} &nbsp;|&nbsp; Best streak: ${maxStreak} &nbsp;|    &nbsp; Time: ${formatTime(timerSeconds)}</strong><br>` +
            `Guesses: ${total} &nbsp;|&nbsp; Correct: ${correct} &nbsp;|&nbsp; Wrong: ${wrong} &nbsp;|&nbsp; Hints: ${hints} &nbsp;|&nbsp; Accuracy: ${accuracy}%`;
    }

    const overlay = document.getElementById('victory-overlay');
    overlay.style.display = 'flex';

    const card = document.getElementById('victory-card');
    card.style.animation = 'none';
    card.offsetHeight;
    card.style.animation = '';
}

function closeVictory() {
    document.getElementById('victory-overlay').style.display = 'none';
}

function loadNextPuzzle() {
    window.location.href = `game.html?difficulty=${diff}`;
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