from pathlib import Path

expected = Path("expected-files.txt").read_text().split()
print([file for file in expected if not Path(file).is_file()])
