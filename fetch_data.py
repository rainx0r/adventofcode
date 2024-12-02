from datetime import datetime
from pathlib import Path
import urllib.request
import os

PROJECT_DIR = Path(__file__).parent
CACHE_DIR = PROJECT_DIR / ".cache"

DATA_DIRECTORIES = [
    "haskell/data",
    "swift/Sources/Data",
    "zig/src/data",
]


def get_aoc_data(day: int, year: int, aoc_session: str):
    CACHE_DIR.mkdir(exist_ok=True)
    cache_filepath = CACHE_DIR / f"{year}_{day:02}.txt"

    if not cache_filepath.exists():
        req = urllib.request.Request(
            f"https://adventofcode.com/{year}/day/{day}/input",
            headers={
                "User-Agent": "Mozilla/5.0",
                "Cookie": f"session={aoc_session}"
            }
        )
        with urllib.request.urlopen(req) as r:
            data = r.read().decode("utf-8")

        with open(cache_filepath, "w") as f:
            f.write(data)

        data = str(data)
    else:
        with open(cache_filepath, "r") as f:
            data = f.read()

    return data


def main() -> None:
    current_date = datetime.today()
    assert current_date.month == 12, "This script only works in December."

    aoc_cookie = os.getenv("AOC_SESSION")
    assert aoc_cookie is not None

    for day in range(1, current_date.day + 1):
        print(f"Fetching Day {day:02}...")
        filename = f"day{day:02}.txt"
        for dir in DATA_DIRECTORIES:
            data_path = PROJECT_DIR / f"{current_date.year}" / dir
            if not (data_path / filename).exists():
                aoc_data = get_aoc_data(day, current_date.year, aoc_cookie)

                filepath = data_path / filename
                filepath.touch()
                with open(filepath, "w") as f:
                    f.write(aoc_data)

    print("Fetched all the data!")


if __name__ == "__main__":
    main()
