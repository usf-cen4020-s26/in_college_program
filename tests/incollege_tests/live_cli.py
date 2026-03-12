"""Interactive replay CLI for the InCollege COBOL binary.

This tool simulates a live interactive program by maintaining a transcript of
line-based inputs. After every new input, it replays the full transcript into
``INPUT.TXT``, runs the COBOL executable, and displays ``OUTPUT.TXT``.
"""

from __future__ import annotations

import subprocess
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path

import typer
from rich.console import Console
from rich.panel import Panel
from rich.table import Table


@dataclass
class ReplayResult:
    """Result of one COBOL replay execution."""

    output_text: str
    return_code: int
    stderr_text: str


@dataclass
class LiveCobolSession:
    """State holder for replaying an input transcript against COBOL."""

    executable: Path
    transcript_path: Path
    work_dir: Path
    timeout_seconds: int = 10
    inputs: list[str] = field(default_factory=list)  # type: ignore

    def load_transcript(self) -> None:
        """Load existing transcript lines, if present."""
        if not self.transcript_path.exists():
            self.inputs = []
            return

        # splitlines() preserves all logical user entries without trailing newline noise.
        self.inputs = self.transcript_path.read_text(encoding="utf-8").splitlines()

    def save_transcript(self) -> None:
        """Persist current transcript to disk."""
        payload = "\n".join(self.inputs)
        if payload:
            payload += "\n"
        self.transcript_path.parent.mkdir(parents=True, exist_ok=True)
        self.transcript_path.write_text(payload, encoding="utf-8")

    def clear_transcript(self) -> None:
        """Reset transcript in memory and on disk."""
        self.inputs = []
        self.save_transcript()

    def append_input(self, value: str) -> None:
        """Add one user input line and persist it."""
        self.inputs.append(value)
        self.save_transcript()

    def undo_last(self) -> bool:
        """Remove the most recent user input line.

        Returns ``True`` when a value was removed.
        """
        if not self.inputs:
            return False

        self.inputs.pop()
        self.save_transcript()
        return True

    def replay(self) -> ReplayResult:
        """Write INPUT.TXT, run executable, and collect OUTPUT.TXT."""
        self.work_dir.mkdir(parents=True, exist_ok=True)

        input_txt_path = self.work_dir / "INPUT.TXT"
        output_txt_path = self.work_dir / "OUTPUT.TXT"

        payload = "\n".join(self.inputs)
        if payload:
            payload += "\n"
        input_txt_path.write_text(payload, encoding="utf-8")

        if output_txt_path.exists():
            output_txt_path.unlink()

        process = subprocess.run(
            [str(self.executable)],
            cwd=self.work_dir,
            capture_output=True,
            text=True,
            timeout=self.timeout_seconds,
            check=False,
        )

        if output_txt_path.exists():
            output_text = output_txt_path.read_text(encoding="utf-8")
        else:
            output_text = process.stdout

        return ReplayResult(
            output_text=output_text,
            return_code=process.returncode,
            stderr_text=process.stderr,
        )


def _render_help(console: Console) -> None:
    table = Table(title="Live Commands", show_header=True)
    table.add_column("Command", style="bold cyan")
    table.add_column("Description", style="white")
    table.add_row(":help", "Show available live commands")
    table.add_row(":dump", "Write inputs + latest replay output to local dump file")
    table.add_row(":show", "Show logged transcript lines")
    table.add_row(":undo", "Remove last logged input line")
    table.add_row(":clear", "Clear all logged input lines")
    table.add_row(":rerun", "Replay current transcript")
    table.add_row(":quit", "Exit the live session")
    console.print(table)


def _render_transcript(console: Console, lines: list[str]) -> None:
    table = Table(title=f"Transcript ({len(lines)} line(s))", show_header=True)
    table.add_column("#", justify="right", style="magenta")
    table.add_column("Input", style="white")

    for idx, value in enumerate(lines, start=1):
        table.add_row(str(idx), value)

    if not lines:
        table.add_row("-", "(empty)")

    console.print(table)


def _render_replay_result(
    console: Console,
    session: LiveCobolSession,
    result: ReplayResult,
) -> None:
    console.clear()
    subtitle = (
        f"Executable: {session.executable} | "
        f"Transcript: {session.transcript_path} | "
        f"Inputs: {len(session.inputs)}"
    )

    console.print(f"[bold green]InCollege Live Replay[/bold green]\\n{subtitle}")
    panel_title = "OUTPUT.TXT Replay"
    console.print(
        Panel(
            result.output_text or "(no output)", title=panel_title, border_style="blue"
        )
    )

    if result.return_code != 0:
        console.print(
            Panel(
                f"Process exited with code {result.return_code}.\\n\\n{result.stderr_text}",
                title="Execution Error",
                border_style="red",
            )
        )


def _write_dump_file(
    session: LiveCobolSession,
    result: ReplayResult,
    dump_file_path: Path,
) -> Path:
    """Persist current transcript and latest replay output to a local text file."""
    dump_file_path.parent.mkdir(parents=True, exist_ok=True)

    lines: list[str] = []
    lines.append("INCOLLEGE LIVE SESSION DUMP")
    lines.append(f"Timestamp: {datetime.now().isoformat(timespec='seconds')}")
    lines.append(f"Executable: {session.executable}")
    lines.append(f"Work dir: {session.work_dir}")
    lines.append(f"Transcript file: {session.transcript_path}")
    lines.append(f"Input line count: {len(session.inputs)}")
    lines.append("")
    lines.append("=== INPUTS ===")

    if session.inputs:
        for idx, value in enumerate(session.inputs, start=1):
            lines.append(f"{idx:03d}: {value}")
    else:
        lines.append("(empty)")

    lines.append("")
    lines.append("=== OUTPUT ===")
    lines.append(result.output_text.rstrip("\n"))

    if result.return_code != 0:
        lines.append("")
        lines.append("=== EXECUTION ERROR ===")
        lines.append(f"Return code: {result.return_code}")
        lines.append(result.stderr_text.rstrip("\n"))

    lines.append("")
    dump_payload = "\n".join(lines)
    dump_file_path.write_text(dump_payload, encoding="utf-8")
    return dump_file_path


def main(
    executable: Path = typer.Argument(..., help="Path to compiled COBOL executable"),
    transcript: Path = typer.Option(
        Path(".live_session.input.txt"),
        "--transcript",
        "-t",
        help="Path to persisted transcript input file",
    ),
    work_dir: Path = typer.Option(
        Path("/tmp/incollege_live_cli"),
        "--work-dir",
        "-w",
        help="Working directory containing INPUT.TXT and OUTPUT.TXT",
    ),
    timeout: int = typer.Option(
        10,
        "--timeout",
        help="Execution timeout in seconds for each replay",
        min=1,
    ),
    fresh: bool = typer.Option(
        False,
        "--fresh",
        help="Ignore any existing transcript and start from an empty session",
    ),
    dump_file: Path = typer.Option(
        Path("live_session_dump.txt"),
        "--dump-file",
        help="Path for :dump snapshots (default: workspace root file)",
    ),
) -> int:
    """Run an interactive COBOL replay session."""
    console = Console()

    resolved_executable = executable.resolve()
    if not resolved_executable.exists():
        console.print(f"[red]Executable not found:[/red] {resolved_executable}")
        return 1

    session = LiveCobolSession(
        executable=resolved_executable,
        transcript_path=transcript.resolve(),
        work_dir=work_dir.resolve(),
        timeout_seconds=timeout,
    )

    if fresh:
        session.clear_transcript()
    else:
        session.load_transcript()

    try:
        result = session.replay()
    except subprocess.TimeoutExpired:
        console.print(f"[red]Initial replay timed out after {timeout} seconds.[/red]")
        return 1

    resolved_dump_file = dump_file.resolve()

    _render_replay_result(console, session, result)
    console.print(
        "Type :help for commands. Enter any other text to log one input line."
    )

    while True:
        raw = typer.prompt("input")
        command = raw.strip()

        if command == ":quit":
            console.print("Session ended.")
            return 0

        if command == ":help":
            _render_help(console)
            continue

        if command == ":dump":
            written_path = _write_dump_file(session, result, resolved_dump_file)
            console.print(f"[green]Snapshot saved:[/green] {written_path}")
            continue

        if command == ":show":
            _render_transcript(console, session.inputs)
            continue

        if command == ":undo":
            removed = session.undo_last()
            if not removed:
                console.print("[yellow]Transcript already empty.[/yellow]")
                continue
        elif command == ":clear":
            should_clear = typer.confirm("Clear all logged inputs?", default=False)
            if not should_clear:
                continue
            session.clear_transcript()
        elif command == ":rerun":
            pass
        else:
            session.append_input(raw)

        try:
            result = session.replay()
        except subprocess.TimeoutExpired:
            console.print(f"[red]Replay timed out after {timeout} seconds.[/red]")
            continue

        _render_replay_result(console, session, result)


if __name__ == "__main__":
    raise SystemExit(main())
