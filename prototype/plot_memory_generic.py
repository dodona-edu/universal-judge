import queue

import matplotlib.pyplot as plt
from jupyter_client.manager import start_new_kernel

from utils.memory_limit import MemoryLimit
from utils.sampler import Sampler
from utils.timeout import Timeout

TIMEOUT = 120  # 2 minutes
MEMORY_LIMIT = 500 * 1024 * 1024  # 500 MiB


def flush_channels(client):
    """
    Flush the channels.
    """
    for channel in (
            client.shell_channel,
            client.iopub_channel,
            client.stdin_channel
    ):
        while True:
            try:
                _ = channel.get_msg(block=True, timeout=0.1)
            except queue.Empty:
                break


def execute_statements(client, kernel_process_id, statements, timeout=TIMEOUT, memory_limit=MEMORY_LIMIT,
                       std_input=None, silent=False, store_history=True):
    flush_channels(client)

    kernel_finished, unprocessed_messages = False, True
    with Timeout(timeout) as timeout:
        with MemoryLimit(memory_limit, kernel_process_id) as mem_limit:
            client.execute(code=statements, silent=silent, store_history=store_history)

            std_input = std_input.splitlines(False) if std_input is not None else []

            while not kernel_finished or unprocessed_messages:
                if timeout.expired:
                    yield 'client', {
                        'msg_type': 'error',
                        'content': {
                            'ename': 'TimeoutError',
                            'evalue': 'time limit exceeded'
                        }
                    }
                    return

                if mem_limit.exceeded:
                    yield 'client', {
                        'msg_type': 'error',
                        'content': {
                            'ename': 'MemoryError',
                            'evalue': 'memory limit exceeded'
                        }
                    }
                    return

                unprocessed_messages = False

                for channel in (
                        client.shell_channel,
                        client.iopub_channel,
                        client.stdin_channel
                ):
                    try:
                        message = channel.get_msg(block=False)
                        unprocessed_messages = True

                        if channel is client.iopub_channel:
                            if message['msg_type'] == 'status':

                                if message['content']['execution_state'] == 'idle':
                                    # report that kernel has finished processing
                                    kernel_finished = True
                                elif message['content']['execution_state'] in {'busy', 'starting'}:
                                    pass
                                else:
                                    yield 'iopub', message

                            elif message['msg_type'] == 'execute_input':

                                # kernel notifies client that it has started executing
                                # the statements
                                pass

                            else:
                                # return the message for further processing
                                yield 'iopub', message
                        elif channel is client.shell_channel:
                            kernel_finished = True
                        else:
                            if message['msg_type'] == 'input_request':

                                if std_input:

                                    # feed the next line of stdin to the kernel
                                    client.input(std_input.pop(0))

                                else:
                                    # kernel code is waiting for input that will never
                                    # be sent by the client
                                    yield 'client', {
                                        'msg_type': 'error',
                                        'content': {
                                            'ename': 'TimeoutError',
                                            'evalue': 'time limit exceeded'
                                        }
                                    }
                                    kernel_finished = True

                            else:
                                # return the message for further processing
                                yield 'stdin', message
                    except queue.Empty:
                        pass


if __name__ == '__main__':
    import sys
    import os

    if len(sys.argv) != 2 and len(sys.argv) != 3:
        print(f'Usage: {sys.argv[0]} <file> [kernel_name]')
        sys.exit(1)

    if not os.path.isfile(sys.argv[1]):
        print(f'{sys.argv[1]} is not a real file')
        sys.exit(2)

    manager, client = start_new_kernel(kernel_name=sys.argv[2] if len(sys.argv) == 3 else 'python3')
    client.allow_stdin = True

    with open(sys.argv[1], 'r') as file:
        contents = file.read()
        split_contents = contents.split('======')

        for content in split_contents:
            for chan, msg in execute_statements(client, manager.kernel.pid, content, timeout=TIMEOUT,
                                                memory_limit=MEMORY_LIMIT):
                if chan == 'iopub':
                    print(str(msg['content']))

        sampler = Sampler(manager.kernel.pid)
        sampler.start()

        for chan, msg in execute_statements(client, manager.kernel.pid,
                                            f'{sys.argv[1].split("/")[-1].replace(".py", "")}()', timeout=TIMEOUT,
                                            memory_limit=MEMORY_LIMIT):
            pass

        sampler.stop()
        samples = sampler.samples

        fig, ax = plt.subplots()

        timestamps = [x[0] for x in samples]
        bytes = [x[1] for x in samples]

        ax.plot([x - min(timestamps) for x in timestamps], [x - min(bytes) for x in bytes], 'o-')
        ax.set(xlabel='time (s)', ylabel='memory usage (bytes)', title='Memory usage over time')
        ax.grid()

        fig.savefig(f'memory_usage_generic_{sys.argv[1].split("/")[-1]}.png')

    client.stop_channels()
    manager.shutdown_kernel()
