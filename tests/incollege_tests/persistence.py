"""COBOL ``.DAT`` file I/O for test seed data.

Provides :class:`PersistenceManager` which reads and writes the fixed-width
record files (``ACCOUNTS.DAT``, ``PROFILES.DAT``, ``PENDING.DAT``,
``CONNECTIONS.DAT``) that the InCollege COBOL program uses for persistence.
"""

from __future__ import annotations

from pathlib import Path

from incollege_tests.constants import (
    ACCOUNT_PASSWORD_WIDTH,
    ACCOUNT_RECORD_WIDTH,
    ACCOUNT_USERNAME_WIDTH,
    CONNECTION_USERNAME_WIDTH,
    MESSAGE_CONTENT_WIDTH,
    MESSAGE_RECORD_WIDTH,
    MESSAGE_TIMESTAMP_WIDTH,
    MESSAGE_USERNAME_WIDTH,
    PROFILE_ABOUT_ME_WIDTH,
    PROFILE_COUNT_WIDTH,
    PROFILE_EDU_DEGREE_WIDTH,
    PROFILE_EDU_MAX,
    PROFILE_EDU_UNIVERSITY_WIDTH,
    PROFILE_EDU_YEARS_WIDTH,
    PROFILE_EXP_COMPANY_WIDTH,
    PROFILE_EXP_DATES_WIDTH,
    PROFILE_EXP_DESC_WIDTH,
    PROFILE_EXP_MAX,
    PROFILE_EXP_TITLE_WIDTH,
    PROFILE_FIRST_NAME_WIDTH,
    PROFILE_GRAD_YEAR_WIDTH,
    PROFILE_HAS_PROFILE_WIDTH,
    PROFILE_LAST_NAME_WIDTH,
    PROFILE_MAJOR_WIDTH,
    PROFILE_RECORD_WIDTH,
    PROFILE_UNIVERSITY_WIDTH,
    PROFILE_USERNAME_WIDTH,
)
from incollege_tests.models import (
    ProfileRecordData,
    SeedConnectionMacro,
    SeedMessageMacro,
    SeedUserMacro,
)


class PersistenceManager:
    """Manages COBOL ``.DAT`` file I/O for seeding test data.

    Args:
        work_dir: Directory where ``.DAT`` files are stored.
    """

    def __init__(self, work_dir: Path) -> None:
        self.work_dir: Path = work_dir
        self.work_dir.mkdir(parents=True, exist_ok=True)
        self.accounts_dat: Path = work_dir / "ACCOUNTS.DAT"
        self.profiles_dat: Path = work_dir / "PROFILES.DAT"
        self.pending_dat: Path = work_dir / "PENDING.DAT"
        self.connections_dat: Path = work_dir / "CONNECTIONS.DAT"
        self.messages_dat: Path = work_dir / "MESSAGES.DAT"
        self.jobs_dat: Path = work_dir / "JOBS.DAT"

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    def seed_users(self, users: list[SeedUserMacro]) -> None:
        """Seed account/profile persistence records before a test executes.

        Existing records for the same username are updated in place.

        Args:
            users: Seed-user macro objects parsed from the test input file.

        Raises:
            ValueError: If seeding would exceed the 5-account limit.
        """
        if not users:
            return

        accounts = self._load_accounts()
        profiles = self._load_profiles()

        account_password_by_username: dict[str, str] = {
            username: password for username, password in accounts
        }
        account_order: list[str] = [username for username, _ in accounts]

        profile_by_username: dict[str, ProfileRecordData] = {
            profile.username: profile for profile in profiles
        }

        for user in users:
            if user.username in account_password_by_username:
                account_password_by_username[user.username] = user.password
            else:
                if len(account_order) >= 5:
                    raise ValueError(
                        "Cannot seed more than 5 accounts. "
                        f"Failed while adding username '{user.username}'."
                    )
                account_order.append(user.username)
                account_password_by_username[user.username] = user.password

            if user.with_profile:
                profile_by_username[user.username] = ProfileRecordData(
                    username=user.username,
                    has_profile="1",
                    first_name=user.first_name,
                    last_name=user.last_name,
                    university=user.university,
                    major=user.major,
                    grad_year=user.grad_year,
                    about_me=user.about_me,
                    exp_count="0",
                    exp_entries=[("", "", "", "") for _ in range(PROFILE_EXP_MAX)],
                    edu_count="0",
                    edu_entries=[("", "", "") for _ in range(PROFILE_EDU_MAX)],
                )

        updated_accounts = [
            (username, account_password_by_username[username])
            for username in account_order
        ]

        updated_profiles = [
            profile_by_username[username]
            for username in account_order
            if username in profile_by_username
        ]

        self._write_accounts(updated_accounts)
        self._write_profiles(updated_profiles)

    def seed_connections(self, connections: list[SeedConnectionMacro]) -> None:
        """Replace ``CONNECTIONS.DAT`` with seeded pairs (deduplicated, undirected key)."""
        if not connections:
            return

        lines: list[str] = []
        seen: set[tuple[str, str]] = set()
        for conn in connections:
            a = conn.user_a.strip()
            b = conn.user_b.strip()
            if not a or not b:
                raise ValueError("@seed_connection requires user_a and user_b.")
            if len(a) > CONNECTION_USERNAME_WIDTH or len(b) > CONNECTION_USERNAME_WIDTH:
                raise ValueError(
                    f"@seed_connection usernames exceed {CONNECTION_USERNAME_WIDTH} characters."
                )
            key = (a, b) if a <= b else (b, a)
            if key in seen:
                continue
            seen.add(key)
            lines.append(
                self._fit(a, CONNECTION_USERNAME_WIDTH)
                + self._fit(b, CONNECTION_USERNAME_WIDTH)
            )

        self.connections_dat.write_text("\n".join(lines) + "\n")

    def seed_messages(self, messages: list[SeedMessageMacro]) -> None:
        """Replace ``MESSAGES.DAT`` with seeded message rows."""
        if not messages:
            return

        lines: list[str] = []
        auto_id = 1
        for msg in messages:
            sid = msg.msg_id if msg.msg_id > 0 else auto_id
            if msg.msg_id <= 0:
                auto_id += 1

            sender = msg.sender.strip()
            recipient = msg.recipient.strip()
            if not sender or not recipient:
                raise ValueError("@seed_message requires sender and recipient.")
            if len(sender) > MESSAGE_USERNAME_WIDTH or len(recipient) > MESSAGE_USERNAME_WIDTH:
                raise ValueError(
                    f"@seed_message sender/recipient exceed {MESSAGE_USERNAME_WIDTH} chars."
                )
            if len(msg.content) > MESSAGE_CONTENT_WIDTH:
                raise ValueError(
                    f"@seed_message content exceeds {MESSAGE_CONTENT_WIDTH} characters."
                )
            if len(msg.timestamp) > MESSAGE_TIMESTAMP_WIDTH:
                raise ValueError(
                    f"@seed_message timestamp exceeds {MESSAGE_TIMESTAMP_WIDTH} characters."
                )

            line = (
                f"{sid:05d}"
                + self._fit(sender, MESSAGE_USERNAME_WIDTH)
                + self._fit(recipient, MESSAGE_USERNAME_WIDTH)
                + self._fit(msg.content, MESSAGE_CONTENT_WIDTH)
                + self._fit(msg.timestamp, MESSAGE_TIMESTAMP_WIDTH)
            )
            if len(line) != MESSAGE_RECORD_WIDTH:
                raise ValueError(
                    f"Internal error: message record length {len(line)} != {MESSAGE_RECORD_WIDTH}."
                )
            lines.append(line)

        self.messages_dat.write_text("\n".join(lines) + "\n")

    def clear_persistence(self) -> None:
        """Delete all ``.DAT`` files to ensure a clean test state."""
        for dat in (
            self.accounts_dat,
            self.profiles_dat,
            self.pending_dat,
            self.connections_dat,
            self.jobs_dat,
            self.work_dir / "APPLICATIONS.DAT",
            self.work_dir / "MESSAGES.DAT",
        ):
            if dat.exists():
                dat.unlink()

    def cleanup_work_dir(self) -> None:
        """Remove all working files (INPUT/OUTPUT.TXT and .DAT files)."""
        for name in (
            "INPUT.TXT",
            "OUTPUT.TXT",
            "ACCOUNTS.DAT",
            "PROFILES.DAT",
            "PENDING.DAT",
            "CONNECTIONS.DAT",
            "MESSAGES.DAT",
            "JOBS.DAT",
            "APPLICATIONS.DAT",
        ):
            p = self.work_dir / name
            if p.exists():
                p.unlink()

    # ------------------------------------------------------------------
    # Internal helpers
    # ------------------------------------------------------------------

    @staticmethod
    def _fit(value: str, width: int) -> str:
        """Trim/pad a string for fixed-width COBOL records."""
        return value[:width].ljust(width)

    def _load_accounts(self) -> list[tuple[str, str]]:
        """Load existing account records from ``ACCOUNTS.DAT``."""
        if not self.accounts_dat.exists():
            return []

        accounts: list[tuple[str, str]] = []
        for raw_line in self.accounts_dat.read_text().splitlines():
            line = raw_line[:ACCOUNT_RECORD_WIDTH].ljust(ACCOUNT_RECORD_WIDTH)
            username = line[:ACCOUNT_USERNAME_WIDTH].rstrip()
            password = line[
                ACCOUNT_USERNAME_WIDTH : ACCOUNT_USERNAME_WIDTH + ACCOUNT_PASSWORD_WIDTH
            ].rstrip()
            if username:
                accounts.append((username, password))
        return accounts

    def _write_accounts(self, accounts: list[tuple[str, str]]) -> None:
        """Write full account dataset to ``ACCOUNTS.DAT``."""
        if not accounts:
            if self.accounts_dat.exists():
                self.accounts_dat.unlink()
            return

        lines: list[str] = []
        for username, password in accounts:
            lines.append(
                self._fit(username, ACCOUNT_USERNAME_WIDTH)
                + self._fit(password, ACCOUNT_PASSWORD_WIDTH)
            )
        self.accounts_dat.write_text("\n".join(lines) + "\n")

    def _load_profiles(self) -> list[ProfileRecordData]:
        """Load existing profile records from ``PROFILES.DAT``."""
        if not self.profiles_dat.exists():
            return []

        profiles: list[ProfileRecordData] = []

        for raw_line in self.profiles_dat.read_text().splitlines():
            line = raw_line[:PROFILE_RECORD_WIDTH].ljust(PROFILE_RECORD_WIDTH)
            cursor = 0

            def take(width: int) -> str:
                nonlocal cursor
                value = line[cursor : cursor + width]
                cursor += width
                return value

            username = take(PROFILE_USERNAME_WIDTH).rstrip()
            has_profile = take(PROFILE_HAS_PROFILE_WIDTH)
            first_name = take(PROFILE_FIRST_NAME_WIDTH)
            last_name = take(PROFILE_LAST_NAME_WIDTH)
            university = take(PROFILE_UNIVERSITY_WIDTH)
            major = take(PROFILE_MAJOR_WIDTH)
            grad_year = take(PROFILE_GRAD_YEAR_WIDTH)
            about_me = take(PROFILE_ABOUT_ME_WIDTH)
            exp_count = take(PROFILE_COUNT_WIDTH)

            exp_entries: list[tuple[str, str, str, str]] = []
            for _ in range(PROFILE_EXP_MAX):
                exp_entries.append(
                    (
                        take(PROFILE_EXP_TITLE_WIDTH),
                        take(PROFILE_EXP_COMPANY_WIDTH),
                        take(PROFILE_EXP_DATES_WIDTH),
                        take(PROFILE_EXP_DESC_WIDTH),
                    )
                )

            edu_count = take(PROFILE_COUNT_WIDTH)
            edu_entries: list[tuple[str, str, str]] = []
            for _ in range(PROFILE_EDU_MAX):
                edu_entries.append(
                    (
                        take(PROFILE_EDU_DEGREE_WIDTH),
                        take(PROFILE_EDU_UNIVERSITY_WIDTH),
                        take(PROFILE_EDU_YEARS_WIDTH),
                    )
                )

            if username:
                profiles.append(
                    ProfileRecordData(
                        username=username,
                        has_profile=has_profile,
                        first_name=first_name,
                        last_name=last_name,
                        university=university,
                        major=major,
                        grad_year=grad_year,
                        about_me=about_me,
                        exp_count=exp_count,
                        exp_entries=exp_entries,
                        edu_count=edu_count,
                        edu_entries=edu_entries,
                    )
                )

        return profiles

    def _write_profiles(self, profiles: list[ProfileRecordData]) -> None:
        """Write full profile dataset to ``PROFILES.DAT``."""
        if not profiles:
            if self.profiles_dat.exists():
                self.profiles_dat.unlink()
            return

        def ensure_exp_entries(
            entries: list[tuple[str, str, str, str]],
        ) -> list[tuple[str, str, str, str]]:
            safe_entries = list(entries[:PROFILE_EXP_MAX])
            while len(safe_entries) < PROFILE_EXP_MAX:
                safe_entries.append(("", "", "", ""))
            return safe_entries

        def ensure_edu_entries(
            entries: list[tuple[str, str, str]],
        ) -> list[tuple[str, str, str]]:
            safe_entries = list(entries[:PROFILE_EDU_MAX])
            while len(safe_entries) < PROFILE_EDU_MAX:
                safe_entries.append(("", "", ""))
            return safe_entries

        lines: list[str] = []
        for profile in profiles:
            exp_entries = ensure_exp_entries(profile.exp_entries)
            edu_entries = ensure_edu_entries(profile.edu_entries)

            line = ""
            line += self._fit(profile.username, PROFILE_USERNAME_WIDTH)
            line += self._fit(profile.has_profile, PROFILE_HAS_PROFILE_WIDTH)
            line += self._fit(profile.first_name, PROFILE_FIRST_NAME_WIDTH)
            line += self._fit(profile.last_name, PROFILE_LAST_NAME_WIDTH)
            line += self._fit(profile.university, PROFILE_UNIVERSITY_WIDTH)
            line += self._fit(profile.major, PROFILE_MAJOR_WIDTH)
            line += self._fit(profile.grad_year, PROFILE_GRAD_YEAR_WIDTH)
            line += self._fit(profile.about_me, PROFILE_ABOUT_ME_WIDTH)
            line += self._fit(profile.exp_count, PROFILE_COUNT_WIDTH)

            for title, company, dates, desc in exp_entries:
                line += self._fit(title, PROFILE_EXP_TITLE_WIDTH)
                line += self._fit(company, PROFILE_EXP_COMPANY_WIDTH)
                line += self._fit(dates, PROFILE_EXP_DATES_WIDTH)
                line += self._fit(desc, PROFILE_EXP_DESC_WIDTH)

            line += self._fit(profile.edu_count, PROFILE_COUNT_WIDTH)

            for degree, university, years in edu_entries:
                line += self._fit(degree, PROFILE_EDU_DEGREE_WIDTH)
                line += self._fit(university, PROFILE_EDU_UNIVERSITY_WIDTH)
                line += self._fit(years, PROFILE_EDU_YEARS_WIDTH)

            lines.append(line)

        self.profiles_dat.write_text("\n".join(lines) + "\n")
