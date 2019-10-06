import atexit
import queue
import sys

import time
from threading import Thread

import zmq
from jupyter_client.manager import start_new_kernel

from utils.memory_limit import MemoryLimit
from utils.timeout import Timeout


def get_queue(num_elements=4, language='python3', allow_stdin=True):
    context_queue = queue.Queue()
    for _ in range(num_elements):
        new_element = JupyterContext(language=language, allow_stdin=allow_stdin)
        new_element.start()
        context_queue.put(new_element)
    return context_queue


def fill_queue(context_queue, num_elements=4, language='python3', allow_stdin=True):
    while context_queue.qsize() < num_elements:
        new_element = JupyterContext(language=language, allow_stdin=allow_stdin)
        new_element.start()
        context_queue.put(new_element)


def clean_queue(context_queue):
    while not context_queue.empty():
        element = context_queue.get()
        element.join()
        element.clean()


class JupyterContext(Thread):
    def __init__(self, language='python3', allow_stdin=True):
        super().__init__()
        self.language = language
        self.allow_stdin = allow_stdin
        self.failed = False

    def run(self):
        self.manager, self.client = start_new_kernel(kernel_name=self.language)
        atexit.register(self.clean)
        self.client.allow_stdin = self.allow_stdin
        # for elem in self.code:
        #     self.execute_statements(elem)
        return self

    def clean(self):
        atexit.unregister(self.clean)
        self.client.stop_channels()
        self.manager.shutdown_kernel()

    def execute_statements(self, statements, timeout, memory_limit, std_input=None, silent=False):
        self.__flush_channels()
        kernel_finished, unprocessed_messages = False, True
        std_input = std_input.splitlines(False) if std_input is not None else []
        with Timeout(timeout) as timeout:
            with MemoryLimit(memory_limit, self.manager.kernel.pid) as memory_limiter:
                start_time = time.perf_counter()
                self.client.execute(code=statements, silent=silent, store_history=False)

                messages = {'iopub': [], 'stdin': [], 'client': []}

                while not kernel_finished or unprocessed_messages:
                    if timeout.expired:
                        messages['client'].append({
                            'msg_type': 'error',
                            'content': {
                                'ename': 'TimeoutError',
                                'evalue': 'time limit exceeded'
                            }
                        })
                        return messages

                    if memory_limiter.exceeded:
                        messages['client'].append({
                            'msg_type': 'error',
                            'content': {
                                'ename': 'MemoryError',
                                'evalue': 'memory limit exceeded'
                            }
                        })
                        return messages

                    if not unprocessed_messages:
                        zmq.select([self.client.iopub_channel.socket, self.client.stdin_channel.socket], [], [],
                                   timeout=1)
                    unprocessed_messages = False

                    for channel in (
                            self.client.iopub_channel,
                            self.client.stdin_channel,
                    ):
                        try:
                            message = channel.get_msg(block=False)
                            unprocessed_messages = True

                            if channel is self.client.iopub_channel:
                                if message['msg_type'] == 'status':
                                    if message['content']['execution_state'] == 'idle':
                                        if not kernel_finished:
                                            total_time = time.perf_counter() - start_time
                                        kernel_finished = True
                                    elif message['content']['execution_state'] in {'starting'}:
                                        pass
                                    elif message['content']['execution_state'] in {'busy'}:
                                        start_time = time.perf_counter()
                                elif message['msg_type'] == 'execute_input':
                                    pass
                                else:
                                    messages['iopub'].append(message)
                            elif message['msg_type'] == 'input_request':
                                if std_input:
                                    self.client.input(std_input.pop(0))
                                else:
                                    messages['client'].append({
                                        'msg_type': 'error',
                                        'content': {
                                            'ename': 'TimeoutError',
                                            'evalue': 'time limit exceeded'
                                        }
                                    })
                                    kernel_finished = True
                            else:
                                messages['stdin'].append(message)
                        except queue.Empty:
                            pass
                return messages

    def __flush_channels(self):
        for channel in (
                self.client.shell_channel,
                self.client.iopub_channel,
                self.client.stdin_channel,
        ):
            while True:
                try:
                    _ = channel.get_msg(block=False)
                except queue.Empty:
                    break


class RunError(RuntimeError):
    def __init__(self, error):
        self.error = error


def run_outside(context, func, args, timeout=None, memory_limit=None):
    for elem in func[:-1]:
        context.execute_statements(elem.format(*args))
    messages = context.execute_statements(
        func[-1].format(*args),
        timeout=timeout, memory_limit=memory_limit)
    for channel in messages:
        for message in messages[channel]:
            if message['msg_type'] == 'error':
                raise RunError(message['content'])
