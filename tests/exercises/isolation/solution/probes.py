import os
import time


def _pid(process):
    if process == "init":
        return 1
    parent = os.getppid()
    if process == "parent":
        return parent
    with open(f"/proc/{parent}/status") as status:
        for line in status:
            if line.startswith("PPid:"):
                return int(line.split()[1])
    raise LookupError(process)


def _attempt(action):
    try:
        action()
    except PermissionError:
        return "denied"
    return "allowed"


def open_output(process):
    # Only opens the output Dodona reads: nothing is written to it.
    return _attempt(lambda: open(f"/proc/{_pid(process)}/fd/1", "w").close())


def send_signal(process):
    return _attempt(lambda: os.kill(_pid(process), 0))


def list_directory(path):
    return _attempt(lambda: os.listdir(path))


def read_file(path):
    return _attempt(lambda: open(path).read())


def write_file(path):
    return _attempt(lambda: open(path, "w").write("probe"))


def sleep(seconds):
    time.sleep(seconds)
    return "awake"


def link_file(target, name):
    os.symlink(target, name)
